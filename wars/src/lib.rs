use std::{convert::Infallible, iter::once, sync::Arc};

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use relooper::{reloop, BranchMode, ShapedBlock};
use syn::{Ident, Lifetime};
use waffle::{
    cfg::CFGInfo, entity::EntityRef, Block, BlockTarget, ExportKind, Func, ImportKind, Memory,
    Module, Operator, Signature, SignatureData, Type, Value,
};
pub mod unswitch;
pub fn mangle_value(a: Value, b: usize) -> Ident {
    if b == 0 {
        format_ident!("{a}")
    } else {
        format_ident!("{a}p{}", b - 1)
    }
}
pub fn bindname(a: &str) -> String {
    let mut v = vec![];
    for k in a.chars() {
        if k.is_alphanumeric() {
            v.push(k)
        } else {
            v.extend(format!("_{}_", k as u32).chars());
        }
    }
    return v.into_iter().collect();
}
// pub trait ImportCfg {
//     fn import(&self, module: &str, name: &str) -> TokenStream;
// }
pub const INTRINSIC: &'static str = "wars_intrinsic/";
impl Opts<Module<'static>> {
    pub fn import(
        &self,
        module: &str,
        name: &str,
        params: impl Iterator<Item = TokenStream>,
    ) -> TokenStream {
        let root = self.crate_path.clone();
        let Some(a) = module.strip_prefix(INTRINSIC) else {
            let id = format_ident!("{}_{}", bindname(module), bindname(name));
            return quote! {
                ctx.#id(#(#params),*)
            };
        };
        quote! {}
    }
    pub fn render_ty(&self, ctx: &TokenStream, ty: Type) -> TokenStream {
        let root = self.crate_path.clone();
        match ty {
            Type::I32 => quote! {u32},
            Type::I64 => quote! {u64},
            Type::F32 => quote! {f32},
            Type::F64 => quote! {f64},
            Type::V128 => quote! {u128},
            Type::TypedFuncRef {
                nullable,
                sig_index,
            } => {
                let data = &self.module.signatures[sig_index];
                let params = data.params.iter().map(|x| self.render_ty(ctx, *x));
                let returns = data.returns.iter().map(|x| self.render_ty(ctx, *x));
                let mut x = quote! {
                    #root::func::Df<#root::_rexport::tuple_list::tuple_list_type!(#(#params),*),#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*),#ctx>
                };
                if nullable {
                    x = quote! {
                        Option<#x>
                    }
                }
                x
            }
            _ => quote! {#root::func::Value<#ctx>},
        }
    }
    pub fn render_generics(&self, ctx: &TokenStream, data: &SignatureData) -> TokenStream {
        let root = self.crate_path.clone();
        let params = data.params.iter().map(|x| self.render_ty(ctx, *x));
        let param_ids = data
            .params
            .iter()
            .enumerate()
            .map(|(a, _)| format_ident!("p{a}"));
        let returns = data.returns.iter().map(|x| self.render_ty(ctx, *x));
        quote! {
            #root::_rexport::tuple_list::tuple_list_type!(#(#params),*),#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)
        }
    }
    pub fn render_fn_sig(&self, name: Ident, data: &SignatureData) -> TokenStream {
        let root = self.crate_path.clone();
        let base = self.name.clone();
        let ctx = quote! {C};
        // let data = &self.module.signatures[sig_index];
        let params = data.params.iter().map(|x| self.render_ty(&ctx, *x));
        let param_ids = data
            .params
            .iter()
            .enumerate()
            .map(|(a, _)| format_ident!("p{a}"));
        let returns = data.returns.iter().map(|x| self.render_ty(&ctx, *x));

        quote! {
            fn #name<'a,C: #base + 'static>(ctx: &'a mut C, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>
        }
    }
    pub fn render_fun_ref(&self, ctx: &TokenStream, x: Func) -> TokenStream {
        let root = self.crate_path.clone();
        if x.is_invalid() {
            return quote! {
                #root::func::da::<(),(),C,_>(|ctx,arg|panic!("invalid func"))
            };
        }
        let generics =
            self.render_generics(ctx, &self.module.signatures[self.module.funcs[x].sig()]);
        let x = format_ident!("{x}");
        quote! {
            #root::func::da::<#generics,C,_>(|ctx,arg|#x(ctx,arg))
        }
    }
    pub fn render_self_sig(
        &self,
        name: Ident,
        wrapped: Ident,
        data: &SignatureData,
    ) -> TokenStream {
        let root = self.crate_path.clone();
        let base = self.name.clone();
        let ctx = quote! {Self};
        // let data = &self.module.signatures[sig_index];
        let params = data.params.iter().map(|x| self.render_ty(&ctx, *x));
        let param_ids = data
            .params
            .iter()
            .enumerate()
            .map(|(a, _)| format_ident!("p{a}"))
            .collect::<Vec<_>>();

        let returns = data.returns.iter().map(|x| self.render_ty(&ctx, *x));

        quote! {
            fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>> where Self: 'static{
                return #wrapped(self,#root::_rexport::tuple_list::tuple_list!(#(#param_ids),*));
            }
        }
    }
    pub fn render_self_sig_import(&self, name: Ident, data: &SignatureData) -> TokenStream {
        let root = self.crate_path.clone();
        let base = self.name.clone();
        let ctx = quote! {Self};
        // let data = &self.module.signatures[sig_index];
        let params = data.params.iter().map(|x| self.render_ty(&ctx, *x));
        let param_ids = data
            .params
            .iter()
            .enumerate()
            .map(|(a, _)| format_ident!("p{a}"))
            .collect::<Vec<_>>();

        let returns = data.returns.iter().map(|x| self.render_ty(&ctx, *x));

        quote! {
            fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>;
        }
    }
    pub fn render_relooped_block(&self, f: Func, x: &ShapedBlock<Block>) -> TokenStream {
        let root = self.crate_path.clone();
        let b = self.module.funcs[f].body().unwrap();
        match x {
            ShapedBlock::Simple(s) => {
                let stmts = s.label;
                fn term(b: &BranchMode) -> TokenStream {
                    match b {
                        relooper::BranchMode::LoopBreak(l) => {
                            let l = Lifetime::new(&format!("'l{}", l), Span::call_site());
                            quote! {
                                break #l;
                            }
                        }
                        relooper::BranchMode::LoopBreakIntoMulti(l) => {
                            let l = Lifetime::new(&format!("'l{}", l), Span::call_site());
                            quote! {
                                break #l;
                            }
                        }
                        relooper::BranchMode::LoopContinue(l) => {
                            let l = Lifetime::new(&format!("'l{}", l), Span::call_site());
                            quote! {
                                continue #l;
                            }
                        }
                        relooper::BranchMode::LoopContinueIntoMulti(l) => {
                            let l = Lifetime::new(&format!("'l{}", l), Span::call_site());
                            quote! {
                                continue #l;
                            }
                        }
                        relooper::BranchMode::MergedBranch => {
                            quote! {}
                        }
                        relooper::BranchMode::MergedBranchIntoMulti => quote! {},
                        relooper::BranchMode::SetLabelAndBreak => quote! {
                            break 'cff;
                        },
                    }
                }
                if stmts.is_invalid() {
                    let immediate = s
                        .immediate
                        .as_ref()
                        .map(|a| self.render_relooped_block(f, a.as_ref()))
                        .unwrap_or_default();
                    let next = s
                        .next
                        .as_ref()
                        .map(|a| self.render_relooped_block(f, a.as_ref()))
                        .unwrap_or_default();
                    let term2 = term(
                        &s.branches
                            .get(&b.entry)
                            .cloned()
                            .unwrap_or(relooper::BranchMode::MergedBranch),
                    );
                    return quote! {
                        #term2;
                        #immediate;
                        #next;
                    };
                }
                let stmts = b.blocks[stmts].params.iter().map(|a|&a.1).chain(b.blocks[stmts].insts.iter()).map(|a|{
                    let av = b.values[*a].tys(&b.type_pool).iter().enumerate().map(|b|mangle_value(*a,b.0));
                    let b = match &b.values[*a]{
                        waffle::ValueDef::BlockParam(k, i, _) => {
                            let a = format_ident!("{k}param{i}");
                            quote! {
                                #root::_rexport::tuple_list::tuple_list!(#a.clone())
                            }
                        },
                        waffle::ValueDef::Operator(o, vals, _) => {
                            let vals = &b.arg_pool[*vals];
                            match o{
                                Operator::I32Const { value } => quote! {
                                    #root::_rexport::tuple_list::tuple_list!(#value)
                                },
                                Operator::I64Const { value } => quote!{
                                    #root::_rexport::tuple_list::tuple_list!(#value)
                                },
                                Operator::Call { function_index } => {
                                    match self.module.funcs[*function_index].body(){
                                        Some(_) => {
                                            let func = format_ident!("{function_index}");
                                            let vals = vals.iter().map(|a|format_ident!("{a}"));
                                            quote! {
                                                match #root::_rexport::tramp::tramp(#func(ctx,#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))){
                                                    Ok(a) => a,
                                                    Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                                }
                                            }
                                        },
                                        None => {
                                            let i = self
                                            .module
                                            .imports
                                            .iter()
                                            .find(|a| a.kind == ImportKind::Func(*function_index))
                                            .unwrap();
                                        let x = self.import(
                                            i.module.as_str(),
                                            i.name.as_str(),
                                            vals.iter().map(|a|format_ident!("{a}")).map(|a| quote! {#a}),
                                        );
                                        quote!{
                                            match #root::_rexport::tramp::tramp(x){
                                                Ok(a) => a,
                                                Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                            }
                                        }
                                        }
                                    }
                                },
                                Operator::CallRef { sig_index } => {
                                    let mut vals = vals.to_owned();
                                    let r = vals.pop().unwrap();
                                        // let func = format_ident!("{function_index}");
                                        let vals = vals.iter().map(|a|format_ident!("{a}"));
                                        let r = format_ident!("{r}");
                                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig_index]);
                                        quote! {
                                            match #root::_rexport::tramp::tramp(#root::func::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))){
                                                Ok(a) => a,
                                                Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                            }
                                        }
                                },
                                Operator::CallIndirect { sig_index, table_index } => {
                                    let t = format_ident!("{table_index}");
                                    let mut vals = vals.to_owned();
                                    let r = vals.pop().unwrap();
                                        // let func = format_ident!("{function_index}");
                                        let vals = vals.iter().map(|a|format_ident!("{a}"));
                                        let r = format_ident!("{r}");
                                        let r = quote! {
                                            ctx.#t()[#r as usize]
                                        };
                                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig_index]);
                                        quote! {
                                            match #root::_rexport::tramp::tramp(#root::func::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))){
                                                Ok(a) => a,
                                                Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                            }
                                        }
                                },
                                Operator::RefFunc { func_index } => {
                                    self.render_fun_ref(&quote! {C},*func_index)
                                },
                                waffle::Operator::MemorySize { mem } => {
                                    let rt = if self.module.memories[*mem].memory64{
                                        quote! {u64}
                                    }else{
                                        quote! {u32}
                                    };
                                    let m = Ident::new(&mem.to_string(), Span::call_site());
                                    quote! {
                                        #root::_rexport::tuple_list::tuple_list!((ctx.#m().len() / 65536) as #rt)
                                    }
                                }
                                waffle::Operator::MemoryGrow { mem } => {
                                    let m = Ident::new(&mem.to_string(), Span::call_site());
                                    let a = vals[0];
                                    let a = format_ident!("{a}");
                                    let rt = if self.module.memories[*mem].memory64{
                                        quote! {u64}
                                    }else{
                                        quote! {u32}
                                    };
                                    quote! {
                                        {
                                        let vn = ctx.#m().len() / 65536;
                                        let l = ctx.#m().len();
                                        ctx.#m().resize(l + (#a .clone() as usize) * 65536,0);
                                        #root::_rexport::tuple_list::tuple_list!(vn as #rt)
                                        }
                                    }
                                },
                                waffle::Operator::MemoryCopy { dst_mem, src_mem } => {
                                    let dst = format_ident!("{dst_mem}");
                                    let src = format_ident!("{src_mem}");
                                    let dst_ptr = format_ident!("{}",vals[0].to_string());
                                    let src_ptr = format_ident!("{}",vals[1].to_string());
                                    let len = format_ident!("{}",vals[2].to_string());
                                    quote!{
                                        {
                                            let m = ctx.#src()[(#src_ptr as usize)..][..(#len as usize)].to_owned();
                                            ctx.#dst()[(#dst_ptr as usize)..][..(#len as usize)].copy_from_slice(&m);
                                        ()
                                        }
                                    }
                                },
                                waffle::Operator::GlobalGet { global_index } => {
                                    let g = Ident::new(&global_index.to_string(), Span::call_site());
                                    quote!{
                                        #root::_rexport::tuple_list::tuple_list!(*ctx.#g())
                                    }
                                }
                                waffle::Operator::GlobalSet { global_index } => {
                                    let g = Ident::new(&global_index.to_string(), Span::call_site());
                                    let r = vals[0];
                                    let r = format_ident!("{r}");
                                    quote!{
                                        {
                                            *ctx.#g() = #r;
                                            ()
                                        }
                                    }
                                }
                                _ if waffle::op_traits::mem_count(o) == 1 => {
                                    let mut mem = Memory::invalid();
                                    waffle::op_traits::rewrite_mem(&mut o.clone(), &mut [();4], |m,_|{
                                        mem = *m;
                                        Ok::<(),Infallible>(())
                                    }).unwrap();
                                    // let clean = o.to_string();
                                    let clean = format_ident!("{}",o.to_string().split_once("<").unwrap().0);
                                    let m2 = mem;
                                    let mem = format_ident!("{mem}");
                                    let mut vals = vals.iter().map(|a|format_ident!("{a}"));
                                    let rt = if self.module.memories[m2].memory64{
                                        quote! {u64}
                                    }else{
                                        quote! {u32}
                                    };
                                    let offset = waffle::op_traits::memory_arg(o).unwrap().offset;
                                    let offset =  if self.module.memories[m2].memory64{
                                        quote! {#offset}
                                    } else{
                                        let offset = offset as u32;
                                        quote! {#offset}
                                    };
                                    let val = vals.next().unwrap();
                                    let vals = once(quote! {(#val.clone() + #offset)}).chain(vals.map(|w|quote!{#w}));
                                    quote! {
                                        match #root::#clean::<#rt>(ctx.#mem(),#(#root::func::cast::<_,_,C>(#vals .clone())),*){
                                            Ok(a) => a,
                                            Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                        }
                                    }
                                },
                                Operator::Select | Operator::TypedSelect { .. } => {
                                    let vals: Vec<_> = vals.iter().map(|a|format_ident!("{a}")).collect();
                                    let cond = vals[0].clone();
                                    let then = vals[1].clone();
                                    let els = vals[2].clone();
                                    quote!{
                                        #root::_rexport::tuple_list::tuple_list!(if #cond != 0{
                                            #then
                                        }else{
                                            #els
                                        })
                                    }
                                },
                                _ => {
                                    // let clean = o.to_string();
                                    let clean = format_ident!("{o}");
                                    let vals = vals.iter().map(|a|format_ident!("{a}"));
                                    quote! {
                                        match #root::#clean(#(#root::func::cast::<_,_,C>(#vals .clone())),*){
                                            Ok(a) => a,
                                            Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Err(e))
                                        }
                                    }
                                }
                            }
                        },
                        waffle::ValueDef::PickOutput(w, i, _) => {
                            let w = mangle_value(*w, *i as usize);
                            quote! {
                                #root::_rexport::tuple_list::tuple_list!(#w)
                            }
                        },
                        waffle::ValueDef::Alias(w) => {
                            let w = format_ident!("{w}");
                            quote! {
                                #root::_rexport::tuple_list::tuple_list!(#w)
                            }
                        },
                        waffle::ValueDef::Placeholder(_) => todo!(),
                        waffle::ValueDef::Trace(_, _) => todo!(),
                        waffle::ValueDef::None => todo!(),
                    };
                    quote! {
                        let #root::_rexport::tuple_list::tuple_list!(#(#av),*) = #b
                    }
                });
                let render_target = |k: &BlockTarget| {
                    let vars = k.args.iter().enumerate().map(|(i, a)| {
                        let a = format_ident!("{a}");
                        let i = format_ident!("{}param{i}", k.block.to_string());
                        quote! {
                            #i = #a;
                        }
                    });
                    let br = term(
                        &s.branches
                            .get(&k.block)
                            .cloned()
                            .unwrap_or(relooper::BranchMode::MergedBranch),
                    );
                    let bi = k.block.index();
                    quote! {
                        #(#vars);*;
                        cff = #bi;
                        #br
                    }
                };
                let term = match &b.blocks[s.label].terminator {
                    waffle::Terminator::Br { target } => render_target(target),
                    waffle::Terminator::CondBr {
                        cond,
                        if_true,
                        if_false,
                    } => {
                        let if_true = render_target(if_true);
                        let if_false = render_target(if_false);
                        let cond = format_ident!("{cond}");
                        quote! {
                            if #cond != 0{
                                #if_true
                            }else{
                                #if_false
                            }
                        }
                    }
                    waffle::Terminator::Select {
                        value,
                        targets,
                        default,
                    } => {
                        let value = format_ident!("{value}");
                        let default = render_target(default);
                        let targets =
                            targets
                                .iter()
                                .map(&render_target)
                                .enumerate()
                                .map(|(a, b)| {
                                    quote! {
                                        #a => {#b}
                                    }
                                });
                        quote! {
                            match #value as usize{
                                #(#targets),*,
                                _ => {#default},
                            }
                        }
                    }
                    waffle::Terminator::Return { values } => {
                        // let values = values.iter().map(|v| format_ident!("{v}"));
                        let values = b.rets.iter().enumerate().map(|(a, _)| match values.get(a) {
                            Some(v) => {
                                let v = format_ident!("{v}");
                                quote! {
                                    #v
                                }
                            }
                            None => {
                                quote! {
                                    ::std::default::Default::default()
                                }
                            }
                        });
                        quote! {
                            return #root::_rexport::tramp::BorrowRec::Ret(Ok(#root::_rexport::tuple_list::tuple_list!(#(#values),*)))
                        }
                    }
                    waffle::Terminator::ReturnCall { func, args } => {
                        match self.module.funcs[*func].body() {
                            Some(_) => {
                                let values = args.iter().map(|v| format_ident!("{v}"));
                                let func = format_ident!("{func}");
                                quote! {
                                    return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                                        #func(ctx,#root::_rexport::tuple_list::tuple_list!(#(#values),*))
                                    }))
                                }
                            }
                            None => {
                                let i = self
                                    .module
                                    .imports
                                    .iter()
                                    .find(|a| a.kind == ImportKind::Func(*func))
                                    .unwrap();
                                let x = self.import(
                                    i.module.as_str(),
                                    i.name.as_str(),
                                    args.iter()
                                        .map(|a| format_ident!("{a}"))
                                        .map(|a| quote! {#a}),
                                );
                                quote! {
                                    return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{#x}))
                                }
                            }
                        }
                    }
                    waffle::Terminator::ReturnCallIndirect { sig, table, args } => {
                        let t = format_ident!("{table}");
                        let mut vals = args.to_owned();
                        let r = vals.pop().unwrap();
                        // let func = format_ident!("{function_index}");
                        let vals = vals.iter().map(|a| format_ident!("{a}"));
                        let r = format_ident!("{r}");
                        let r = quote! {
                            ctx.#t()[#r as usize]
                        };
                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig]);
                        quote! {
                            return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                            #root::func::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))
                        }))
                        }
                    }
                    waffle::Terminator::ReturnCallRef { sig, args } => {
                        let mut vals = args.clone();
                        let r = vals.pop().unwrap();
                        // let func = format_ident!("{function_index}");
                        let vals = vals.iter().map(|a| format_ident!("{a}"));
                        let r = format_ident!("{r}");
                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig]);
                        quote! {
                            return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                            #root::func::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))
                        }))
                        }
                    }
                    waffle::Terminator::Unreachable => quote! {
                        unreachable!()
                    },
                    waffle::Terminator::None => panic!("none block terminator"),
                };

                let immediate = s
                    .immediate
                    .as_ref()
                    .map(|a| self.render_relooped_block(f, a.as_ref()))
                    .unwrap_or_default();
                let next = s
                    .next
                    .as_ref()
                    .map(|a| self.render_relooped_block(f, a.as_ref()))
                    .unwrap_or_default();
                quote! {
                    #(#stmts);*;
                    #term;
                    #immediate;
                    #next;
                }
            }
            ShapedBlock::Loop(l) => {
                let r = self.render_relooped_block(f, &l.inner.as_ref());
                let next = l
                    .next
                    .as_ref()
                    .map(|a| self.render_relooped_block(f, a.as_ref()))
                    .unwrap_or_default();
                let l = Lifetime::new(&format!("'l{}", l.loop_id), Span::call_site());
                quote! {
                    #l : loop{
                        #r
                    };
                    #next;
                }
            }
            ShapedBlock::Multiple(k) => {
                let initial = k.handled.iter().enumerate().flat_map(|(a, b)| {
                    b.labels.iter().map(move |l| {
                        let l = l.index();
                        quote! {
                            #l => #a
                        }
                    })
                });
                let cases = k.handled.iter().enumerate().map(|(a, i)| {
                    let ib = self.render_relooped_block(f, &i.inner);
                    let ic = if i.break_after {
                        quote! {}
                    } else {
                        quote! {
                            cff2 += 1;
                            continue 'cff
                        }
                    };
                    quote! {
                        #a => {
                            #ib;
                            #ic;
                        }
                    }
                });
                quote! {
                    let mut cff2 = match cff{
                        #(#initial),*,
                        _ => unreachable!()
                    };
                    'cff: loop{
                        match cff2{
                            #(#cases),*,
                            _ => unreachable!()
                        };
                        break 'cff;
                    };
                }
            }
        }
    }
    pub fn render_fn(&self, f: Func) -> TokenStream {
        let name = format_ident!("{f}");
        let sig = self.render_fn_sig(
            name.clone(),
            &self.module.signatures[self.module.funcs[f].sig()],
        );
        let root = self.crate_path.clone();
        let Some(b) = self.module.funcs[f].body() else {
            let fsig = self.module.funcs[f].sig();
            let fsig = &self.module.signatures[fsig];
            let params = fsig
                .params
                .iter()
                .enumerate()
                .map(|(a, _)| format_ident!("p{a}"));
            let i = self
                .module
                .imports
                .iter()
                .find(|a| a.kind == ImportKind::Func(f))
                .unwrap();
            let x = self.import(
                i.module.as_str(),
                i.name.as_str(),
                params.map(|a| quote! {#a}),
            );
            return quote! {
                #sig {
                    return #x;
                }
            };
        };
        let cfg = CFGInfo::new(b);
        // let values = b.values.entries().flat_map(|(a, d)| {
        //     return d
        //         .tys(&b.type_pool)
        //         .iter()
        //         .map(move |ty| self.render_ty(&quote! {c}, *ty))
        //         .chain(once(quote! {
        //             () = ()
        //         }))
        //         .enumerate()
        //         .map(move |(i, ty)| {
        //             let a = mangle_value(a, i);
        //             quote! {
        //                 #a: #ty
        //             }
        //         });
        // });
        let bpvalues = b.blocks.entries().flat_map(|(k, d)| {
            d.params.iter().enumerate().map(move |(i, (ty, _))| {
                let x = match ty {
                    Type::TypedFuncRef {
                        nullable,
                        sig_index,
                    } if !nullable => self.render_fun_ref(&quote! {C}, Func::invalid()),
                    _ => quote! {
                        Default::default()
                    },
                };
                let ty = self.render_ty(&quote! {c}, ty.clone());
                let a = format_ident!("{k}param{i}");
                if k == b.entry {
                    let p = format_ident!("p{i}");
                    quote! {
                        #a: #ty = #p
                    }
                } else {
                    quote! {
                        #a: #ty = #x
                    }
                }
            })
        });
        let reloop = std::panic::catch_unwind(|| {
            reloop(
                b.blocks
                    .entries()
                    .filter(|k| cfg.dominates(b.entry, k.0))
                    .map(|(k, l)| {
                        (
                            k,
                            l.succs
                                .iter()
                                .cloned()
                                .chain(b.blocks.iter().filter(|x| cfg.dominates(*x, k)))
                                .collect(),
                        )
                    })
                    .chain(once((Block::invalid(), vec![b.entry])))
                    .collect(),
                Block::invalid(),
            )
        });
        let reloop = match reloop {
            Ok(a) => a,
            Err(e) => {
                panic!(
                    "reloop failure ({}) in {}",
                    e.downcast_ref::<&str>()
                        .map(|a| *a)
                        .unwrap_or("unknown panic"),
                    b.display("", None)
                );
            }
        };
        let x = self.render_relooped_block(f, reloop.as_ref());
        quote! {
            #sig {
                let mut cff: usize = 0;
                #(let mut #bpvalues);*;
                #x;
                panic!("should have returned");
            }
        }
    }
}
pub struct Opts<B> {
    pub crate_path: syn::Path,
    pub module: B,
    pub name: Ident,
    // pub cfg: Arc<dyn ImportCfg>,
}
pub fn go(opts: &Opts<Vec<u8>>) -> proc_macro2::TokenStream {
    let mut module = waffle::Module::from_wasm_bytes(&opts.module, &Default::default()).unwrap();
    module.expand_all_funcs().unwrap();
    let mut module = module.without_orig_bytes();
    // module.per_func_body(|b|unswitch::go(b)); //TODO: reloop better and make it not needed
    // eprintln!("{}",module.display());
    module.per_func_body(|f| f.convert_to_max_ssa(None));
    let internal_path = format_ident!("_{}_internal", opts.name);
    let data = format_ident!("{}Data", opts.name);
    let name = opts.name.clone();
    let opts = Opts {
        crate_path: opts.crate_path.clone(),
        module,
        name: name.clone(),
        // cfg: opts.cfg.clone(),
    };
    let root = opts.crate_path.clone();
    let funcs = opts.module.funcs.iter().map(|a| opts.render_fn(a));
    let mut z = vec![];
    let mut fields = vec![];
    let mut fs = vec![];
    let mut init = vec![];
    for (t, d) in opts.module.tables.entries() {
        // let dty = opts.render_ty(&quote! {Target}, d.ty.clone());
        let n = Ident::new(&t.to_string(), Span::call_site());
        z.push(quote! {
            #n: Vec<#root::func::Value<Target>>
        });
        fields.push(n.clone());
        if let Some(e) = d.func_elements.as_ref() {
            let e = e.iter().map(|x| opts.render_fun_ref(&quote! {C}, *x));
            init.push(quote! {
                #(ctx.data().#n.push(#root::func::Coe::coe(#e)));*;
            })
        }
        fs.push(quote! {
            fn #n(&mut self) -> &mut Vec<#root::func::Value<Self>>{
                &mut self.data().#n
            }
        })
    }
    // eprintln!("before globals");
    for (g, d) in opts.module.globals.entries() {
        let n = Ident::new(&g.to_string(), Span::call_site());
        let t = opts.render_ty(&quote! {Target}, d.ty.clone());
        z.push(quote! {
            #n : #t
        });
        fields.push(n.clone());
        fs.push(quote! {
            fn #n<'a>(&'a mut self) -> &'a mut #t{
                return &mut self.data().#n;
            }
        });
        if let Some(v) = d.value.clone() {
            init.push(quote! {
                *ctx.#n() = (#v as #t);
            })
        }
    }
    for (me, d) in opts.module.memories.entries() {
        // let mut p = vec![];
        // let mut pk = 0;
        // for s in d.segments.clone() {
        //     let f = s.clone().offset;
        //     pk = pk.max(s.data.len() + f);
        //     p.resize(p.len().max(pk + 1), 0);
        //     for (i, d) in s.data.into_iter().enumerate() {
        //         p[i + f] = d;
        //     }
        // }
        let mut import = None;
        for imp in opts.module.imports.iter() {
            if imp.kind == ImportKind::Memory(me) {
                import = Some((imp.module.clone(), imp.name.clone()));
            }
        }
        let n = Ident::new(&me.to_string(), Span::call_site());
        match import {
            None => {
                z.push(quote! {
                    #n : Vec<u8>
                });
                fields.push(n.clone());
                fs.push(quote! {
                    fn #n<'a>(&'a mut self) -> &'a mut Vec<u8>{
                        return &mut self.data().#n;
                    }
                });
            }
            Some((a, b)) => {
                // let a = bindname(&a);
                // let b = bindname(&b);
                let m = Ident::new(&format!("{a}_{b}"), Span::call_site());
                fs.push(quote! {
                    fn #m<'a>(&'a mut self) -> &'a mut Vec<u8>;
                    fn #n<'a>(&'a mut self) -> &'a mut Vec<u8>{
                        return self.#m();
                    }
                });
            }
        }
        let pk = d.initial_pages * 65536;
        init.push(quote! {
            let l = #pk.max(ctx.#n().len());
            ctx.#n().resize(l,0);
        });
        for s in d.segments.clone() {
            for (i, d) in s.data.chunks(65536).enumerate() {
                let o = s.offset + i * 65536;
                let pk = o + d.len();
                let pk = pk + 1;
                // let mut out = out_dir();
                // let mut h = ::sha3::Sha3_256::new();
                // h.update(&p);
                // let h = h.finalize();
                // let h = format!("{:x}.segment", h);
                // out.push(h);
                // std::fs::write(out.clone(), p)?;
                // let out_str = out.to_str().unwrap().to_owned();
                // eprintln!("emuitting data");
                init.push(quote! {
                    ctx.#n()[#o..#pk].copy_from_slice(&[#(#d),*])
                });
            }
        }
    }
    for xp in opts.module.exports.iter() {
        match &xp.kind {
            ExportKind::Func(f) => {
                let f = *f;
                let d = opts.render_self_sig(
                    format_ident!("{}", xp.name),
                    format_ident!("{f}"),
                    &opts.module.signatures[opts.module.funcs[f].sig()],
                );
                fs.push(quote! {
                    #d
                });
            }
            ExportKind::Table(t) => {
                let d = &opts.module.tables[*t];
                let tt = opts.render_ty(&quote! {Self}, d.ty);
                let x = Ident::new(&t.to_string(), Span::call_site());
                let mn = Ident::new(&xp.name, Span::call_site());
                let i = quote! {
                    fn #mn(&mut self) -> &mut Vec<#tt>{
                        return &mut self.z().#x;
                    }
                };
                fs.push(i);
            }
            ExportKind::Global(g) => {
                let d = &opts.module.globals[*g];
                let t = opts.render_ty(&quote! {Self}, d.ty);
                let x = Ident::new(&g.to_string(), Span::call_site());
                let mn = Ident::new(&xp.name, Span::call_site());
                let i = quote! {
                    fn #mn(&mut self) -> &mut #t{
                        return self.#x()
                    }
                };
                fs.push(i);
            }
            ExportKind::Memory(m) => {
                let x = Ident::new(&m.to_string(), Span::call_site());
                let mn = Ident::new(&xp.name, Span::call_site());
                let i = quote! {
                    fn #mn(&mut self) -> &mut Vec<u8>{
                        return self.#x()
                    }
                };
                fs.push(i);
            }
        }
    }
    for i in opts.module.imports.iter() {
        if i.module.starts_with(INTRINSIC) {
            continue;
        }
        if let ImportKind::Func(f) = &i.kind {
            let name = format_ident!("{}_{}", bindname(&i.module), bindname(&i.name));
            fs.push(opts.render_self_sig_import(
                name,
                &opts.module.signatures[opts.module.funcs[*f].sig()],
            ));
        }
    }
    quote! {
        mod #internal_path{
            #(#funcs)*
            pub struct #data<Target: #name + ?Sized>{
                #(#z),*
            }
            pub trait #name: #root::func::CtxSpec{
                fn data(&mut self) -> &mut #data<Self>;
                #(#fs)*

            }
            pub fn init<C: #name>(ctx: &mut C) -> #root::_rexport::anyhow::Result<()>{
                #(#init);*;
                return Ok(())
            }
        }
        use #internal_path::{#name,#data};
    }
}
