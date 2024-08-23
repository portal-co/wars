use core::{
    iter::{empty, once},
};
use alloc::boxed::Box;
// use std::vec::Vec;
use alloc::{
    sync::Arc,
    vec::Vec,
    vec
};

use anyhow::Context;
use tramp::{tramp, BorrowRec, Thunk};
pub mod unsync;
pub fn ret<'a, T>(a: T) -> BorrowRec<'a, T> {
    BorrowRec::Ret(a)
}
pub use crate::CtxSpec;
use crate::Traverse;
pub enum Value<C: CtxSpec> {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    FunRef(
        Arc<
            dyn for<'a> Fn(
                    &'a mut C,
                    Vec<Value<C>>,
                ) -> tramp::BorrowRec<'a, anyhow::Result<Vec<Value<C>>>>
                + Send
                + Sync
                + 'static,
        >,
    ),
    Null,
    ExRef(C::ExternRef),
}
impl<C: CtxSpec> Traverse<C> for Value<C> {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a <C as CtxSpec>::ExternRef> + 'a> {
        match self {
            Value::ExRef(e) => Box::new(once(e)),
            _ => Box::new(empty()),
        }
    }

    fn traverse_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <C as CtxSpec>::ExternRef> + 'a> {
        match self {
            Value::ExRef(e) => Box::new(once(e)),
            _ => Box::new(empty()),
        }
    }
}
pub fn call_ref<'a, A: CoeVec<C> + 'static, B: CoeVec<C> + 'static, C: CtxSpec + 'static>(
    ctx: &'a mut C,
    go: Df<A, B, C>,
    a: A,
) -> tramp::BorrowRec<'a, anyhow::Result<B>> {
    // let go: Df<A, B, C> = cast(go);
    go(ctx, a)
}

impl<C: CtxSpec> Clone for Value<C> {
    fn clone(&self) -> Self {
        match self {
            Self::I32(arg0) => Self::I32(arg0.clone()),
            Self::I64(arg0) => Self::I64(arg0.clone()),
            Self::F32(arg0) => Self::F32(arg0.clone()),
            Self::F64(arg0) => Self::F64(arg0.clone()),
            Self::FunRef(arg0) => Self::FunRef(arg0.clone()),
            Self::Null => Self::Null,
            Self::ExRef(e) => Self::ExRef(e.clone()),
        }
    }
}
pub trait Coe<C: CtxSpec>: Sized {
    fn coe(self) -> Value<C>;
    fn uncoe(x: Value<C>) -> anyhow::Result<Self>;
}
pub fn cast<A: Coe<C> + 'static, B: Coe<C> + 'static, C: CtxSpec>(a: A) -> B {
    let a = match castaway::cast!(a, B) {
        Ok(b) => return b,
        Err(a) => a,
    };
    B::uncoe(A::coe(a)).unwrap()
}
impl<C: CtxSpec> Coe<C> for Value<C> {
    fn coe(self) -> Value<C> {
        self
    }

    fn uncoe(x: Value<C>) -> anyhow::Result<Self> {
        Ok(x)
    }
}
impl<C: CtxSpec, D: Coe<C>> Coe<C> for Option<D> {
    fn coe(self) -> Value<C> {
        match self {
            None => Value::Null,
            Some(d) => d.coe(),
        }
    }

    fn uncoe(x: Value<C>) -> anyhow::Result<Self> {
        if let Value::Null = x {
            return Ok(None);
        }
        return Ok(Some(D::uncoe(x)?));
    }
}
macro_rules! coe_impl_prim {
    ($a:tt in $b:ident) => {
        impl<C: CtxSpec> Coe<C> for $a {
            fn coe(self) -> Value<C> {
                Value::$b(self)
            }
            fn uncoe(x: Value<C>) -> anyhow::Result<Self> {
                match x {
                    Value::$b(a) => Ok(a),
                    _ => anyhow::bail!("invalid type"),
                }
            }
        }
    };
}
coe_impl_prim!(u32 in I32);
coe_impl_prim!(u64 in I64);
coe_impl_prim!(f32 in F32);
coe_impl_prim!(f64 in F64);
pub trait CoeVec<C: CtxSpec>: Sized {
    fn coe(self) -> Vec<Value<C>>;
    fn uncoe(a: Vec<Value<C>>) -> anyhow::Result<Self>;
}
impl<C: CtxSpec> CoeVec<C> for () {
    fn coe(self) -> Vec<Value<C>> {
        vec![]
    }

    fn uncoe(a: Vec<Value<C>>) -> anyhow::Result<Self> {
        Ok(())
    }
}
impl<C: CtxSpec, A: Coe<C>, B: CoeVec<C>> CoeVec<C> for (A, B) {
    fn coe(self) -> Vec<Value<C>> {
        let mut a = self.1.coe();
        a.push(self.0.coe());
        return a;
    }

    fn uncoe(mut a: Vec<Value<C>>) -> anyhow::Result<Self> {
        let Some(x) = a.pop() else {
            anyhow::bail!("list too small")
        };
        let y = A::uncoe(x).context("invalid item (note coe lists are REVERSED)")?;
        let z = B::uncoe(a)?;
        Ok((y, z))
    }
}
pub fn map_rec<'a, T: 'a, U>(
    r: BorrowRec<'a, T>,
    go: impl FnOnce(T) -> U + 'a,
) -> BorrowRec<'a, U> {
    match r {
        BorrowRec::Ret(a) => BorrowRec::Ret(go(a)),
        BorrowRec::Call(t) => BorrowRec::Call(Thunk::new(move || {
            let t = t.compute();
            map_rec(t, go)
        })),
    }
}
pub type Df<A, B, C> = Arc<
    dyn for<'a> Fn(&'a mut C, A) -> tramp::BorrowRec<'a, anyhow::Result<B>> + Send + Sync + 'static,
>;

pub fn da<
    A,
    B,
    C,
    F: for<'a> Fn(&'a mut C, A) -> tramp::BorrowRec<'a, anyhow::Result<B>> + Send + Sync + 'static,
>(
    f: F,
) -> Df<A, B, C> {
    Arc::new(f)
}

impl<C: CtxSpec + 'static, A: CoeVec<C> + 'static, B: CoeVec<C> + 'static> Coe<C> for Df<A, B, C> {
    fn coe(self) -> Value<C> {
        pub fn x<
            C: CtxSpec,
            T: for<'a> Fn(
                    &'a mut C,
                    Vec<Value<C>>,
                ) -> tramp::BorrowRec<'a, anyhow::Result<Vec<Value<C>>>>
                + Send
                + Sync
                + 'static,
        >(
            a: T,
        ) -> T {
            return a;
        }
        Value::FunRef(Arc::new(x(move |ctx, x| {
            let x = match A::uncoe(x) {
                Ok(x) => x,
                Err(e) => return BorrowRec::Ret(Err(e)),
            };
            let x = self(ctx, x);
            map_rec(x, |a| a.map(|b| b.coe()))
        })))
    }

    fn uncoe(x: Value<C>) -> anyhow::Result<Self> {
        let Value::FunRef(x) = x else {
            anyhow::bail!("invalid value")
        };
        Ok(Arc::new(move |ctx, a| {
            let v = a.coe();
            let v = x(ctx, v);
            map_rec(v, |a| a.and_then(B::uncoe))
        }))
    }
}
pub trait Call<A, B, C>:
    for<'a> Fn(&'a mut C, A) -> tramp::BorrowRec<'a, anyhow::Result<B>> + 'static
{
    fn call(&self, c: &mut C, a: A) -> anyhow::Result<B>;
}
impl<A, B, C, T: for<'a> Fn(&'a mut C, A) -> tramp::BorrowRec<'a, anyhow::Result<B>> + 'static>
    Call<A, B, C> for T
{
    fn call(&self, c: &mut C, a: A) -> anyhow::Result<B> {
        tramp((self)(c, a))
    }
}
