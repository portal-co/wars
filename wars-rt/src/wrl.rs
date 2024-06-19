use std::sync::Arc;

use wasm_runtime_layer::{AsContextMut, ExternRef, FuncType};

use crate::func::CtxSpec;
#[derive(Clone)]
pub enum MetaType {
    I32,
    I64,
    F32,
    F64,
    ExternRef,
    FunRef {
        params: Vec<MetaType>,
        returns: Vec<MetaType>,
    },
}
impl MetaType {
    fn wrl(&self) -> wasm_runtime_layer::ValueType {
        match self {
            MetaType::I32 => wasm_runtime_layer::ValueType::I32,
            MetaType::I64 => wasm_runtime_layer::ValueType::I64,
            MetaType::F32 => wasm_runtime_layer::ValueType::F32,
            MetaType::F64 => wasm_runtime_layer::ValueType::F64,
            MetaType::ExternRef => wasm_runtime_layer::ValueType::ExternRef,
            MetaType::FunRef { params, returns } => wasm_runtime_layer::ValueType::FuncRef,
        }
    }
}
pub fn translate_in<C: CtxSpec + AsContextMut<UserState = D> + 'static, D: AsMut<C>>(
    val: &crate::func::Value<C>,
    ctx: &mut C,
    wrl_ty: &MetaType,
) -> anyhow::Result<wasm_runtime_layer::Value>
where
    C::ExternRef: Send + Sync + 'static,
{
    Ok(match val {
        crate::Value::I32(a) => wasm_runtime_layer::Value::I32(*a as i32),
        crate::Value::I64(a) => wasm_runtime_layer::Value::I64(*a as i64),
        crate::Value::F32(a) => wasm_runtime_layer::Value::F32(*a),
        crate::Value::F64(a) => wasm_runtime_layer::Value::F64(*a),
        crate::Value::FunRef(f) => {
            // let wasm_runtime_layer::ValueType::FuncRef()
            let MetaType::FunRef { params, returns } = wrl_ty.clone() else {
                unreachable!()
            };
            let ty = FuncType::new(
                params.iter().map(|a| a.wrl()),
                returns.iter().map(|a| a.wrl()),
            );
            let f = f.clone();
            wasm_runtime_layer::Value::FuncRef(Some(wasm_runtime_layer::Func::new(
                ctx,
                ty,
                move |mut ctx, args, rets| {
                    let args2 = args
                        .iter()
                        .zip(params.iter())
                        .rev()
                        .map(|(x, y)| translate_out(x, ctx.data_mut().as_mut(), y))
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    let v = tramp::tramp(f(ctx.data_mut().as_mut(), args2))?;
                    for ((w, v), t) in v.iter().rev().zip(rets.iter_mut()).zip(returns.iter()) {
                        *v = translate_in(w, ctx.data_mut().as_mut(), t)?;
                    }

                    Ok(())
                },
            )))
        }
        crate::Value::Null => match wrl_ty{
            MetaType::ExternRef => wasm_runtime_layer::Value::ExternRef(None),
            MetaType::FunRef { params, returns } => wasm_runtime_layer::Value::FuncRef(None),
            _ => anyhow::bail!("invalid null")
        },
        crate::Value::ExRef(e) => {
            wasm_runtime_layer::Value::ExternRef(Some(ExternRef::new(ctx, e.clone())))
        }
    })
}
pub fn translate_out<C: CtxSpec + AsContextMut<UserState = D> + 'static, D: AsMut<C>>(
    val: &wasm_runtime_layer::Value,
    ctx: &mut C,
    wrl_ty: &MetaType,
) -> anyhow::Result<crate::func::Value<C>>
where
    C::ExternRef: Send + Sync + 'static,
{
    Ok(match val {
        wasm_runtime_layer::Value::I32(i) => crate::Value::I32(*i as u32),
        wasm_runtime_layer::Value::I64(i) => crate::Value::I64(*i as u64),
        wasm_runtime_layer::Value::F32(f) => crate::Value::F32(*f),
        wasm_runtime_layer::Value::F64(f) => crate::Value::F64(*f),
        wasm_runtime_layer::Value::FuncRef(f) => match f {
            None => crate::Value::Null,
            Some(a) => {
                let a = a.clone();
                let MetaType::FunRef { params, returns } = wrl_ty.clone() else {
                    unreachable!()
                };
                crate::Value::FunRef(Arc::new(move |ctx, args| {
                    let args_in: anyhow::Result<Vec<_>> = args
                        .iter()
                        .rev()
                        .zip(params.iter())
                        .map(|(x, y)| translate_in(x, &mut *ctx, y))
                        .collect();
                    let mut results =
                        vec![wasm_runtime_layer::Value::I32(0); a.ty(&mut *ctx).results().len()];
                    let args_in = match args_in {
                        Ok(a) => a,
                        Err(e) => return tramp::BorrowRec::Ret(Err(e)),
                    };
                    tramp::BorrowRec::Ret(match a.call(&mut *ctx, &args_in, &mut results) {
                        Err(e) => Err(e),
                        Ok(_) => results
                            .iter()
                            .zip(returns.iter())
                            .rev()
                            .map(|(x, y)| translate_out(x, ctx, y))
                            .collect(),
                    })
                }))
            }
        },
        wasm_runtime_layer::Value::ExternRef(x) => match x
            .as_ref()
            .and_then(|a| a.downcast::<C::ExternRef, _, _>(ctx.as_context()).ok())
        {
            None => crate::Value::Null,
            Some(x) => crate::Value::ExRef(x.clone()),
        },
    })
}
