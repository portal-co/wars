use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    f32::consts::E,
    iter::once,
    sync::Arc,
};


use proc_macro2::{Span, TokenStream};
use quasiquote::quasiquote;
use quote::{format_ident, quote, ToTokens};
use relooper::{reloop, BranchMode, ShapedBlock};
use sha3::Digest;
use syn::{Ident, Lifetime};
use waffle::{
    cfg::CFGInfo, entity::EntityRef, Block, BlockTarget, Export, ExportKind, Func, ImportKind,
    Memory, Module, Operator, Signature, SignatureData, Type, Value,
};
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Flags: u32{
        const HOST_MEMORY = 0x1;
        const ASYNC = 0x2;
        const LEGACY = 0x4;
        const WASIX = 0x8;
        const BIND = 0x10;
        const PIT = 0x20;
        // const UNSANDBOXED = 0x2;
    }
}
pub mod unswitch;
// pub mod wasix;
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
    pub fn fp(&self) -> TokenStream {
        let root = self.crate_path.clone();
        if self.flags.contains(Flags::ASYNC) {
            quote! {
                #root::func::unsync
            }
        } else {
            quote! {
                #root::func
            }
        }
    }
    pub fn mem(&self, m: Memory) -> TokenStream {
        if let Some(i) = self
            .module
            .imports
            .iter()
            .find(|x| x.kind == ImportKind::Memory(m))
        {
            if i.module == "!!unsafe" && i.name == "host" && self.flags.contains(Flags::HOST_MEMORY)
            {
                return quote! {
                    unsafe{
                        ::std::slice::from_raw_parts_mut(::std::ptr::null(),usize::MAX)
                    }
                };
            }
            if self.flags.contains(Flags::WASIX) {
                if i.module == "wasi_snapshot_preview1" || i.module == "wasix_32v1" {
                    if i.name == "memory" {
                        return quote! {
                            ctx.wasix_memory()
                        };
                    }
                }
            }
        }
        let m2 = format_ident!("{m}");
        quote! {
            ctx.#m2()
        }
    }
    pub fn import(
        &self,
        module: &str,
        name: &str,
        mut params: impl Iterator<Item = TokenStream>,
    ) -> TokenStream {
        let root = self.crate_path.clone();
        // if self.flags.contains(Flags::UNSANDBOXED) {
        if self.flags.contains(Flags::HOST_MEMORY) {
            if module == "wars/memory/host" {
                match name {
                    _ => {}
                }
            }
        }
        if self.flags.contains(Flags::WASIX) {
            if module == "wasi_snapshot_preview1" || module == "wasix_32v1" {
                return quasiquote! {
                    #root::wasix::#{format_ident!("{name}")}(#(#params),*)
                };
            }
        }
        if self.flags.contains(Flags::BIND) {
            if module == "wars/bind" {
                if name == "!!drop" {
                    return quasiquote! {
                        {
                            let v = self.data().rust_table.remove(#(#params),*).unwrap();
                            Ok(())
                        }
                    };
                }
                let params = params.collect::<Vec<_>>();
                return quasiquote! {
                    {
                        let v = #{syn::parse_str::<TokenStream>(&name).unwrap()}(#(unsafe{#root::get_cell(self.data().rust_table.get(&#params)).unwrap()}),*)#{if self.flags.contains(Flags::ASYNC){
                            quote!{.await}
                        }else{
                            quote!{}
                        }};
                        v.map(|x|(alloc(&mut self.data().rust_table,#root::any_cell(x)),()))
                    }
                };
            }
        }
        //     if a == "fs" {
        //         match name {
        //             "open" => {
        //                 let p0 = params.next().unwrap();
        //                 let l = params.next().unwrap();
        //                 return quote! {
        //                     #root::_rexport::tramp::BorrowRec::Ret({
        //                         let f = #p0;
        //                         let l = #l;
        //                         let f = ::std::str::from_utf8(&ctx.memory()[(f as usize)..][..(l as usize)]);
        //                         let f = match f{
        //                             Ok(a) => a,
        //                             Err(e) => return #root::_rexport::tramp::BorrowRec::Ret(Er(e.into()));
        //                         };
        //                         let f = ::std::fs::open(f);
        //                         let f = match f{
        //                             Ok(a) =>  alloc(&mut ctx.data().files,::std::sync::Arc::new(a)) * 2,
        //                             Err(e) => alloc(&mut ctx.data().io_errors,::std::sync::Arc::new(e)) * 2 + 1;
        //                         };
        //                         f
        //                     })
        //                 };
        //             }
        //             "read" => {
        //                 let fd = params.next().unwrap();
        //                 let p0 = params.next().unwrap();
        //                 let l = params.next().unwrap();
        //                 return quote! {
        //                     {
        //                         let f = #p0;
        //                         let l = #l;
        //                         let fd = #fd;
        //                         let fd = ctx.data().files.get(&fd).unwrap().clone();
        //                         let f = &mut ctx.memory()[(f as usize)..][..(l as usize)];
        //                         let f = ::std::io::Read::read(&mut fd.as_ref(),f);
        //                         match f{
        //                             Ok(a) =>                               #root::_rexport::tuple_list::tuple_list!(a as u64 * 2),
        //                             Err(e) => #root::_rexport::tuple_list::tuple_list!(alloc(&mut ctx.data().io_errors,::std::sync::Arc::new(e)) as u64 * 2 + 1);
        //                         }
        //                     }
        //                 };
        //             }
        //             "write" => {
        //                 let fd = params.next().unwrap();
        //                 let p0 = params.next().unwrap();
        //                 let l = params.next().unwrap();
        //                 return quote! {
        //                     {
        //                         let f = #p0;
        //                         let l = #l;
        //                         let fd = #fd;
        //                         let fd = ctx.data().files.get(&fd).unwrap().clone();
        //                         let f = &ctx.memory()[(f as usize)..][..(l as usize)];
        //                         let f = ::std::io::Write::write(&mut fd.as_ref(),f);
        //                         match f{
        //                             Ok(a) =>                               #root::_rexport::tuple_list::tuple_list!(a as u64 * 2),
        //                             Err(e) => #root::_rexport::tuple_list::tuple_list!(alloc(&mut ctx.data().io_errors,::std::sync::Arc::new(e)) as u64 * 2 + 1);
        //                         }
        //                     }
        //                 };
        //             }
        //             _ => {}
        //         }
        //     }
        // }
        if self.flags.contains(Flags::PIT) {
            if let Some(i) = module.strip_prefix("pit/") {
                let x: [u8; 32] = hex::decode(i).unwrap().try_into().unwrap();
                if let Some(s) = name.strip_prefix("~") {
                    let s = {
                        let mut h = sha3::Sha3_256::default();
                        h.update(s);
                        h.finalize()
                    };
                    return quasiquote! {
                        #{self.fp()}::ret(Ok(#{self.fp()}::Value::<C>::ExternRef(#root::Pit{
                            id: [#(#x),*],
                            x: #{self.fp()}::CoeVec::coe(#root::tuple_list::tuple_list!(#(#params),*)),
                            s: [#(#s),*],
                        }.into())))
                    };
                }

                let mut f = params.next().unwrap();
                // let id = format_ident!("{}", bindname(&format!("pit/{i}/~{PIT_NS}/{name}")));
                // ctx.#id(x.x,#(#params),*)
                let params = params.collect::<Vec<_>>();
                let cases = self
                    .module
                    .exports
                    .iter()
                    .filter_map(|x| x.name.strip_prefix(&format!("pit/{i}/~")))
                    .filter_map(|x| x.strip_suffix(&format!("/{name}")))
                    .map(|s| {
                        let id = format_ident!("{}", bindname(&format!("pit/{i}/~{s}/{name}")));
                        let s = {
                            let mut h = sha3::Sha3_256::default();
                            h.update(s);
                            h.finalize()
                        };
                        quasiquote! {
                            [#(#s),*] => {
                                let mut y = #{self.fp()}::CoeVec::coe(#root::tuple_list::tuple_list!(#(#params),*));
                                y.extend(&mut x.x.clone());
                                ctx.#id(#{self.fp()}::CoeVec::uncoe(y))
                            }
                        }
                    });
                return quasiquote! {
                    'a: {
                        let x = #f;
                        let #{self.fp()}::Value::<C>::ExternRef(x) = x else{
                            break 'a #{self.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("not an externref")))
                        };
                        let Ok(x) = x.try_into() else{
                            break 'a #{self.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("not a pit externref")))
                        };
                        let x: #root::Pit = x;
                        match x.s{
                            #(#cases),*,
                            _ => break 'a #{self.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("invalid target")))
                        }
                    }
                };
            }
            if module == "pit" && name == "drop" {
                let mut f = params.next().unwrap();
                let cases = self
                    .module
                    .exports
                    .iter()
                    .filter_map(|x| {
                        let x = x.name.as_str();
                        let x = x.strip_prefix("pit/")?;
                        let (a, x) = x.split_once("/~")?;
                        let s = x.strip_suffix(".drop")?;
                        return Some((a, s));
                    })
                    .map(|(a, s)| {
                        let x = hex::decode(a).unwrap();
                        let id = format_ident!("{}", bindname(&format!("pit/{a}/~{s}.drop")));
                        let s = {
                            let mut h = sha3::Sha3_256::default();
                            h.update(s);
                            h.finalize()
                        };
                        // let id = format_ident!(
                        //     "{}",
                        //     bindname(&format!("pit/{}/~{PIT_NS}.drop", i.rid_str()))
                        // ); ctx.#id(x.x)
                        quasiquote!(
                            ([#(#x),*],[#(#s),*]) => ctx.#id(#{self.fp()}::CoeVec::uncoe(x.x))
                        )
                    });
                return quasiquote! {
                    'a: {
                        let x = #f;
                        let #{self.fp()}::Value::<C>::ExternRef(x) = x else{
                            break 'a #{self.fp()}::ret(Ok(()));
                        };
                        if let Ok(x) = x.try_into(){
                            let x: #root::Pit = x;
                            break 'a match (x.id,x.s){
                                #(#cases),*,
                                _ => #{self.fp()}::ret(Ok(()))
                            }
                        }else{
                            break 'a #{self.fp()}::ret(Ok(()))
                        }
                    }
                };
            }
        };
        let id = format_ident!("{}_{}", bindname(module), bindname(name));
        return quote! {
            ctx.#id(#(#params),*)
        };
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
                let mut x = if self.flags.contains(Flags::ASYNC) {
                    quote! {
                        #root::func::unsync::Df<#root::_rexport::tuple_list::tuple_list_type!(#(#params),*),#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*),#ctx>
                    }
                } else {
                    quote! {
                        #root::func::Df<#root::_rexport::tuple_list::tuple_list_type!(#(#params),*),#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*),#ctx>
                    }
                };
                if nullable {
                    x = quote! {
                        Option<#x>
                    }
                }
                x
            }
            _ => quasiquote! {#{self.fp()}::Value<#ctx>},
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
        let mut x = if self.flags.contains(Flags::ASYNC) {
            quote! {
                fn #name<'a,C: #base + 'static>(ctx: &'a mut C, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::func::unsync::AsyncRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>
            }
        } else {
            quote! {
                fn #name<'a,C: #base + 'static>(ctx: &'a mut C, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>
            }
        };
        if let Some(t) = self.roots.get("tracing") {
            x = quote! {
                #[#t::instrument]
                #x
            };
        }
        return x;
    }
    pub fn fname(&self, a: Func) -> Ident {
        format_ident!("{a}_{}", bindname(self.module.funcs[a].name()))
    }
    pub fn render_fun_ref(&self, ctx: &TokenStream, x: Func) -> TokenStream {
        let root = self.crate_path.clone();
        if x.is_invalid() {
            return quasiquote! {
                #{self.fp()}::da::<(),(),C,_>(|ctx,arg|panic!("invalid func"))
            };
        }
        let generics =
            self.render_generics(ctx, &self.module.signatures[self.module.funcs[x].sig()]);
        let x = self.fname(x);
        quasiquote! {
            #{self.fp()}::da::<#generics,C,_>(|ctx,arg|#x(ctx,arg))
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
        if self.flags.contains(Flags::ASYNC) {
            quote! {
                fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::func::unsync::AsyncRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>> where Self: 'static{
                    return #wrapped(self,#root::_rexport::tuple_list::tuple_list!(#(#param_ids),*));
                }
            }
        } else {
            quote! {
                fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>> where Self: 'static{
                    return #wrapped(self,#root::_rexport::tuple_list::tuple_list!(#(#param_ids),*));
                }
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
        if self.flags.contains(Flags::ASYNC) {
            quote! {
                fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::func::unsync::AsyncRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>;
            }
        } else {
            quote! {
                fn #name<'a>(self: &'a mut Self, #root::_rexport::tuple_list::tuple_list!(#(#param_ids),*): #root::_rexport::tuple_list::tuple_list_type!(#(#params),*)) -> #root::_rexport::tramp::BorrowRec<'a,#root::_rexport::anyhow::Result<#root::_rexport::tuple_list::tuple_list_type!(#(#returns),*)>>;
            }
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
                let fp = self.fp();
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
                                            let func = self.fname(*function_index);
                                            let vals = vals.iter().map(|a|format_ident!("{a}"));
                                            quasiquote! {
                                                {
                                                    let x = #func(ctx,#root::_rexport::tuple_list::tuple_list!(#(#fp::cast::<_,_,C>(#vals .clone())),*));
                                                    match #{if self.flags.contains(Flags::ASYNC){
                                                        quote!{
                                                            x.go().await
                                                        }
                                                    }else{
                                                        quote!{
                                                            #root::_rexport::tramp::tramp(x)
                                                        }
                                                    }}{
                                                        Ok(a) => a,
                                                        Err(e) => return #{self.fp()}::ret(Err(e))
                                                    }
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
                                        quasiquote!{
                                            match #{if self.flags.contains(Flags::ASYNC){
                                                quote!{
                                                    #x.go().await
                                                }
                                            }else{
                                                quote!{
                                                    #root::_rexport::tramp::tramp(#x)
                                                }
                                            }}{
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            }
                                        }
                                        }
                                    }
                                },
                                Operator::CallRef { sig_index } => {
                                    let mut vals = vals.to_owned();
                                    let r = vals.pop().expect(" a ref to call");
                                        // let func = format_ident!("{function_index}");
                                        let vals = vals.iter().map(|a|format_ident!("{a}"));
                                        let r = format_ident!("{r}");
                                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig_index]);
                                        quasiquote! {
                                            {
                                            let x = #{self.fp()}::call_ref::<#g,C>(ctx,#{self.fp()}(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#fp::cast::<_,_,C>(#vals .clone())),*));
                                            match #{if self.flags.contains(Flags::ASYNC){
                                                quote!{
                                                    x.go().await
                                                }
                                            }else{
                                                quote!{
                                                    #root::_rexport::tramp::tramp(x)
                                                }
                                            }}{
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            }
                                        }
                                        }
                                },
                                Operator::CallIndirect { sig_index, table_index } => {
                                    let t = format_ident!("{table_index}");
                                    let mut vals = vals.to_owned();
                                    let r = vals.pop().expect("a table index to call");
                                        // let func = format_ident!("{function_index}");
                                        let vals = vals.iter().map(|a|format_ident!("{a}"));
                                        let r = format_ident!("{r}");
                                        let r = quote! {
                                            ctx.#t()[#r as usize]
                                        };
                                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig_index]);
                                        quasiquote! {
                                            {
                                            let r = #r.clone();
                                            let x = #{self.fp()}::call_ref::<#g,C>(ctx,#{self.fp()}::cast(r),#root::_rexport::tuple_list::tuple_list!(#(#fp::cast::<_,_,C>(#vals .clone())),*));
                                            match #{if self.flags.contains(Flags::ASYNC){
                                                quote!{
                                                    x.go().await
                                                }
                                            }else{
                                                quote!{
                                                    #root::_rexport::tramp::tramp(x)
                                                }
                                            }}{
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            }
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
                                    quasiquote! {
                                        #root::_rexport::tuple_list::tuple_list!(((match #root::Memory::size(ctx.#m()){
                                            Ok(a) => a,
                                            Err(e) => return #{self.fp()}::ret(Err(e))
                                        }) / 65536) as #rt)
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
                                    quasiquote! {
                                        {
                                        let vn = (match #root::Memory::size(ctx.#m()){
                                            Ok(a) => a,
                                            Err(e) => return #{self.fp()}::ret(Err(e))
                                        }) / 65536;
                                        match #root::Memory::grow(ctx.#m(),(#a .clone() as u64) * 65536){
                                            Ok(a) => a,
                                            Err(e) => return #{self.fp()}::ret(Err(e))
                                        };
                                        #root::_rexport::tuple_list::tuple_list!(vn as #rt)
                                        }
                                    }
                                },
                                waffle::Operator::MemoryCopy { dst_mem, src_mem } => {
                                    let dst = self.mem(*dst_mem);
                                    let src = self.mem(*src_mem);
                                    let dst_ptr = format_ident!("{}",vals[0].to_string());
                                    let src_ptr = format_ident!("{}",vals[1].to_string());
                                    let len = format_ident!("{}",vals[2].to_string());
                                    quasiquote!{
                                        {
                                            let m = match #src.read(#src_ptr as u64,#len as u64){
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            }.as_ref().as_ref().to_owned();
                                            match #dst.write(#dst_ptr as u64,&m){
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            };
                                        ()
                                        }
                                    }
                                },
                                waffle::Operator::MemoryFill { mem } => {
                                    let dst = self.mem(*mem);
                                    // let src = self.mem(*src_mem);
                                    let dst_ptr = format_ident!("{}",vals[0].to_string());
                                    let val = format_ident!("{}",vals[1].to_string());
                                    let len = format_ident!("{}",vals[2].to_string());
                                    quasiquote!{
                                        {
                                            let m = vec![(#val & 0xff) as u8; #len as u64];
                                            match #dst.write(#dst_ptr as u64,&m){
                                                Ok(a) => a,
                                                Err(e) => return #{self.fp()}::ret(Err(e))
                                            };
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
                                Operator::TableGet { table_index } => {
                                    let table = format_ident!("{table_index}");
                                    let [i,..] = vals else{
                                        unreachable!()
                                    };
                                    let i = format_ident!("{i}");
                                    // let j = format_ident!("{j}");
                                    quasiquote!{
                                        {
                                        (ctx.#table()[#i as usize].clone(),())
                                        }
                                    }
                                },
                                Operator::TableSet { table_index } => {
                                    let table = format_ident!("{table_index}");
                                    let [i,j,..] = vals else{
                                        unreachable!()
                                    };
                                    let i = format_ident!("{i}");
                                    let j = format_ident!("{j}");
                                    quasiquote!{
                                        {
                                        ctx.#table()[#i as usize] = #{self.fp()}::cast::<_,_,C>(#j.clone());
                                        ()
                                        }
                                    }
                                },
                                Operator::TableSize { table_index } => {
                                    let table = format_ident!("{table_index}");
                                    quote!{
                                        (ctx.#table().len() as u32,())
                                    }
                                },
                                Operator::TableGrow { table_index } => {
                                    let table = format_ident!("{table_index}");
                                    let [i,j,..] = vals else{
                                        unreachable!()
                                    };
                                    let i = format_ident!("{i}");
                                    let j = format_ident!("{j}");
                                    quasiquote!{
                                        {
                                            for _ in 0..#i{
                                                ctx.#table().push(#{self.fp()}::cast::<_,_,C>(#j.clone()));
                                            }
                                            ()
                                        }
                                    }
                                },
                                _ if waffle::op_traits::mem_count(o) == 1 => {
                                    let mut mem = Memory::invalid();
                                    waffle::op_traits::rewrite_mem(&mut o.clone(), &mut [();4], |m,_|{
                                        mem = *m;
                                        Ok::<(),Infallible>(())
                                    }).expect("wut");
                                    // let clean = o.to_string();
                                    let clean = format_ident!("{}",o.to_string().split_once("<").expect("a memory op").0);
                                    let m2 = mem;
                                    let mem = self.mem(m2);
                                    let mut vals = vals.iter().map(|a|format_ident!("{a}"));
                                    let rt = if self.module.memories[m2].memory64{
                                        quote! {u64}
                                    }else{
                                        quote! {u32}
                                    };
                                    let offset = waffle::op_traits::memory_arg(o).expect(&format!("a memory arg from {}",o)).offset;
                                    
                                    let offset =  if self.module.memories[m2].memory64{
                                        quote! {#offset}
                                    } else{
                                        let offset = offset as u32;
                                        quote! {#offset}
                                    };
                                    let val = vals.next().expect("the runtime memory offset");
                                    let vals = once(quote! {(#val.clone() + #offset)}).chain(vals.map(|w|quote!{#w}));
                                    quasiquote! {
                                        match #root::#clean::<#rt,_>(#mem,#(#fp::cast::<_,_,C>(#vals .clone())),*){
                                            Ok(a) => a,
                                            Err(e) => return #{self.fp()}::ret(Err(e))
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
                                            #els.into()
                                        })
                                    }
                                },
                                _ => {
                                    // let clean = o.to_string();
                                    let clean = format_ident!("{o}");
                                    let vals = vals.iter().map(|a|format_ident!("{a}"));
                                    quasiquote! {
                                        match #root::#clean(#(#fp::cast::<_,_,C>(#vals .clone())),*){
                                            Ok(a) => a,
                                            Err(e) => return #{self.fp()}::ret(Err(e))
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
                        quasiquote! {
                            #i = #{self.fp()}::cast::<_,_,C>(#a);
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
                                quasiquote! {
                                    #{self.fp()}::cast::<_,_,C>(#v)
                                }
                            }
                            None => {
                                quote! {
                                    ::std::default::Default::default()
                                }
                            }
                        });
                        quasiquote! {
                            return #{self.fp()}::ret(Ok(#root::_rexport::tuple_list::tuple_list!(#(#values),*)))
                        }
                    }
                    waffle::Terminator::ReturnCall { func, args } => {
                        match self.module.funcs[*func].body() {
                            Some(_) => {
                                let values = args.iter().map(|v| format_ident!("{v}")).map(|a| {
                                    quasiquote! {
                                        #{self.fp()}::cast::<_,_,C>(#a)
                                    }
                                });
                                let func = self.fname(*func);
                                if self.flags.contains(Flags::ASYNC) {
                                    quote! {
                                        #func(ctx,#root::_rexport::tuple_list::tuple_list!(#(#values),*))
                                    }
                                } else {
                                    quote! {
                                        return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                                            #func(ctx,#root::_rexport::tuple_list::tuple_list!(#(#values),*))
                                        }))
                                    }
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
                                if self.flags.contains(Flags::ASYNC) {
                                    x
                                } else {
                                    quote! {
                                        return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{#x}))
                                    }
                                }
                            }
                        }
                    }
                    waffle::Terminator::ReturnCallIndirect { sig, table, args } => {
                        let t = format_ident!("{table}");
                        let mut vals = args.to_owned();
                        let r = vals.pop().expect("a table index to call");
                        // let func = format_ident!("{function_index}");
                        let vals = vals.iter().map(|a| format_ident!("{a}")).map(|a| {
                            quasiquote! {
                                #{self.fp()}::cast::<_,_,C>(#a)
                            }
                        });
                        let r = format_ident!("{r}");
                        let r = quote! {
                            ctx.#t()[#r as usize]
                        };
                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig]);
                        if self.flags.contains(Flags::ASYNC) {
                            quasiquote! {
                                return #{self.fp()}::call_ref::<#g,C>(ctx,#{self.fp()}::cast(r),#root::_rexport::tuple_list::tuple_list!(#(#{self.fp()}::cast::<_,_,C>(#vals .clone())),*))
                            }
                        } else {
                            quasiquote! {
                                    let r = #r.clone();
                                    return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                                    #{self.fp()}::call_ref::<#g,C>(ctx,#{self.fp()}::cast(r),#root::_rexport::tuple_list::tuple_list!(#(#{self.fp()}::cast::<_,_,C>(#vals .clone())),*))
                                }))
                            }
                        }
                    }
                    waffle::Terminator::ReturnCallRef { sig, args } => {
                        let mut vals = args.clone();
                        let r = vals.pop().expect(" a ref to call");
                        // let func = format_ident!("{function_index}");
                        let vals = vals.iter().map(|a| format_ident!("{a}")).map(|a| {
                            quasiquote! {
                                #{self.fp()}::cast::<_,_,C>(#a)
                            }
                        });
                        let r = format_ident!("{r}");
                        let g = self.render_generics(&quote! {c}, &self.module.signatures[*sig]);
                        if self.flags.contains(Flags::ASYNC) {
                            quasiquote! {
                                return #{self.fp()}::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))
                            }
                        } else {
                            quasiquote! {
                                    return #root::_rexport::tramp::BorrowRec::Call(#root::_rexport::tramp::Thunk::new(move||{
                                    #{self.fp()}::call_ref::<#g,C>(ctx,#root::func::cast(#r.clone()),#root::_rexport::tuple_list::tuple_list!(#(#root::func::cast::<_,_,C>(#vals .clone())),*))
                                }))
                            }
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
        let name = self.fname(f);
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
        let reloop = waffle_func_reloop::go(b);
        // let reloop = std::panic::catch_unwind(|| {
        //     reloop(
        //         b.blocks
        //             .entries()
        //             .filter(|k| cfg.dominates(b.entry, k.0))
        //             .map(|(k, l)| {
        //                 (
        //                     k,
        //                     l.succs
        //                         .iter()
        //                         .cloned()
        //                         .chain(b.blocks.iter().filter(|x| cfg.dominates(*x, k)))
        //                         .collect(),
        //                 )
        //             })
        //             .chain(once((Block::invalid(), vec![b.entry])))
        //             .collect(),
        //         Block::invalid(),
        //     )
        // });
        // let reloop = match reloop {
        //     Ok(a) => a,
        //     Err(e) => {
        //         panic!(
        //             "reloop failure ({}) in {}",
        //             e.downcast_ref::<&str>()
        //                 .map(|a| *a)
        //                 .unwrap_or("unknown panic"),
        //             b.display("", None)
        //         );
        //     }
        // };
        let x = self.render_relooped_block(f, reloop.as_ref());
        let mut b = quote! {
            let mut cff: usize = 0;
            #(let mut #bpvalues);*;
            #x;
            panic!("should have returned");
        };
        if self.flags.contains(Flags::ASYNC) {
            b = quasiquote! {
                return #{self.fp()}::AsyncRec::Async(Box::pin(async move{
                    #b
                }))
            }
        }
        quote! {
            #sig {
                #b
            }
        }
    }
}
#[derive(Clone)]
pub struct Opts<B> {
    pub crate_path: syn::Path,
    pub module: B,
    pub name: Ident,
    pub flags: Flags,
    pub embed: TokenStream,
    pub data: BTreeMap<Ident, TokenStream>,
    pub roots: BTreeMap<String, TokenStream>,
    // pub cfg: Arc<dyn ImportCfg>,
}
impl<X: AsRef<[u8]>> Opts<X> {
    pub fn to_mod(&self) -> Opts<Module<'static>> {
        let opts = self;
        let mut module =
            waffle::Module::from_wasm_bytes(opts.module.as_ref(), &Default::default()).unwrap();
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
            flags: opts.flags,
            embed: opts.embed.clone(),
            data: opts.data.clone(),
            roots: opts.roots.clone(),
            // cfg: opts.cfg.clone(),
        };
        return opts;
    }
}
impl ToTokens for Opts<Module<'static>> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        go(self).to_tokens(tokens)
    }
}
pub fn go(opts: &Opts<Module<'static>>) -> proc_macro2::TokenStream {
    // let mut opts = opts.clone();
    // let is = if opts.flags.contains(Flags::PIT) {
    //     pit_patch::get_interfaces(&opts.module)
    //         .unwrap()
    //         .into_iter()
    //         .collect::<BTreeSet<_>>()
    // } else {
    //     Default::default()
    // };
    // if opts.flags.contains(Flags::PIT) {
    //     for x in is.iter() {
    //         pit_patch::canon::canon(&mut opts.module, &x.rid_str(), PIT_NS).unwrap();
    //     }
    // }
    // let mut module = waffle::Module::from_wasm_bytes(&opts.module, &Default::default()).unwrap();
    // module.expand_all_funcs().unwrap();
    // let mut module = module.without_orig_bytes();
    // // module.per_func_body(|b|unswitch::go(b)); //TODO: reloop better and make it not needed
    // // eprintln!("{}",module.display());
    // module.per_func_body(|f| f.convert_to_max_ssa(None));
    let internal_path = format_ident!("_{}_internal", opts.name);
    let data = format_ident!("{}Data", opts.name);
    let name = opts.name.clone();
    // let opts = Opts {
    //     crate_path: opts.crate_path.clone(),
    //     module,
    //     name: name.clone(),
    //     flags: opts.flags,
    //     // cfg: opts.cfg.clone(),
    // };
    let root = opts.crate_path.clone();
    let funcs = opts.module.funcs.iter().map(|a| opts.render_fn(a));
    let mut z = vec![];
    let mut fields = vec![];
    let mut sfields = vec![];
    let mut fs = vec![];
    fs.push(opts.embed.clone());
    for (k, v) in opts.data.iter() {
        fields.push(k.clone());
        z.push(quote! {
            #k : #v
        });
    }
    if (opts.flags.contains(Flags::BIND)) {
        let k = format_ident!("rust_table");
        fields.push(k.clone());
        z.push(quote! {
            #k: ::std::collections::BTreeMap<u32,AnyCell>,
        })
    }
    let mut init = vec![];
    for (t, d) in opts.module.tables.entries() {
        // let dty = opts.render_ty(&quote! {Target}, d.ty.clone());
        let n = Ident::new(&t.to_string(), Span::call_site());
        z.push(quasiquote! {
            #n: Vec<#{opts.fp()}::Value<Target>>
        });
        fields.push(n.clone());
        sfields.push(n.clone());
        if let Some(e) = d.func_elements.as_ref() {
            let e = e.iter().map(|x| opts.render_fun_ref(&quote! {C}, *x));
            init.push(if opts.flags.contains(Flags::ASYNC) {
                quote! {
                    #(ctx.data().#n.push(#root::func::unsync::Coe::coe(#e)));*;
                }
            } else {
                quote! {
                    #(ctx.data().#n.push(#root::func::Coe::coe(#e)));*;
                }
            })
        }
        fs.push(quasiquote! {
            fn #n(&mut self) -> &mut Vec<#{opts.fp()}::Value<Self>>{
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
        sfields.push(n.clone());
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
                let mut t = quote! {
                    Vec<u8>
                };
                if d.shared {
                    t = quote! {
                        ::std::sync::Arc<::std::lock::Mutex<#t>>
                    };
                };
                z.push(quote! {
                    #n : #t
                });
                fields.push(n.clone());
                fs.push(quote! {
                    fn #n<'a>(&'a mut self) -> &'a mut #t{
                        return &mut self.data().#n;
                    }
                });
            }
            Some((a, b)) => {
                if a == "!!unsafe" && b == "host" && opts.flags.contains(Flags::HOST_MEMORY) {
                } else if (a == "wasi_snapshot_preview1" || a == "wasix_32v1")
                    && b == "memory"
                    && opts.flags.contains(Flags::WASIX)
                {
                } else if a.starts_with("pit") && opts.flags.contains(Flags::PIT) {
                } else {
                    // let a = bindname(&a);
                    // let b = bindname(&b);
                    let m = Ident::new(&format!("{a}_{b}"), Span::call_site());
                    let mut p = if opts.flags.contains(Flags::LEGACY) {
                        quote! {dyn #root::Memory + 'a}
                    } else {
                        quote! {
                            impl #root::Memory + 'a
                        }
                    };
                    if d.shared {
                        p = quote! {
                            ::std::sync::Arc<::std::lock::Mutex<#p>>
                        };
                    };
                    fs.push(quote! {
                        fn #m<'a>(&'a mut self) -> &'a mut (#p);
                        fn #n<'a>(&'a mut self) -> &'a mut (#p){
                            return self.#m();
                        }
                    });
                }
            }
        }
        let pk = d.initial_pages * 65536;
        init.push(quote! {
            let l = #pk.max(ctx.#n().size()?);
            let s = ctx.#n().size()?;
            ctx.#n().grow(l - s)?;
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
                    ctx.#n().write(#o,&[#(#d),*])?
                });
            }
        }
    }
    for xp in opts.module.exports.iter() {
        let xp = Export {
            name: bindname(&xp.name),
            kind: xp.kind.clone(),
        };
        match &xp.kind {
            ExportKind::Func(f) => {
                let f = *f;
                let d = opts.render_self_sig(
                    format_ident!("{}", xp.name),
                    opts.fname(f),
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
                let i = quasiquote! {
                    fn #mn<'a>(&'a mut self) -> &'a mut (#{
                        let mut p = if opts.flags.contains(Flags::LEGACY) {
                            quote! {dyn #root::Memory + 'a}
                        } else {
                            quote! {
                                impl #root::Memory + 'a
                            }
                        };
                        if opts.module.memories[*m].shared{
                            p = quote!{
                                ::std::sync::Arc<::std::lock::Mutex<#p>>
                            };
                        };
                        p
                    }){
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
        if opts.flags.contains(Flags::WASIX) {
            if i.module == "wasi_snapshot_preview1" || i.module == "wasix_32v1" {
                continue;
            }
        }
        if opts.flags.contains(Flags::BIND) {
            if i.module == "wars/bind" {
                continue;
            }
        }
        if opts.flags.contains(Flags::PIT) {
            if i.module.starts_with("pit") {
                continue;
            }
        }
        if let ImportKind::Func(f) = &i.kind {
            let name = format_ident!("{}_{}", bindname(&i.module), bindname(&i.name));
            fs.push(opts.render_self_sig_import(
                name,
                &opts.module.signatures[opts.module.funcs[*f].sig()],
            ));
        }
    }
    // if opts.flags.contains(Flags::UNSANDBOXED) {
    //     for (x, y) in [
    //         (
    //             format_ident!("files"),
    //             quote! {
    //                 ::std::collections::BTreeMap<u32,::std::sync::Arc<::std::fs::File>>
    //             },
    //         ),
    //         (
    //             format_ident!("io_errors"),
    //             quote! {
    //                 ::std::collections::BTreeMap<u32,::std::sync::Arc<::std::io::Error>>
    //             },
    //         ),
    //     ] {
    //         fields.push(x.clone());
    //         z.push(quote! {
    //             #x: #y
    //         })
    //     }
    // }
    let defaults = fields.iter().map(|a| {
        quote! {
            #a: Default::default()
        }
    });
    let clones = fields.iter().map(|a| {
        quote! {
            #a: self.#a.clone()
        }
    });
    quasiquote! {
        mod #internal_path{
            #(#funcs)*
            use #root::Memory;
            use #root::AnyCell;
            pub fn alloc<T>(m: &mut ::std::collections::BTreeMap<u32,T>, x: T) -> u32{
                let mut u = 0;
                while m.contains_key(&u){
                    u += 1;
                };
                m.insert(u,x);
                return u;
            }
            pub struct #data<Target: #name + ?Sized>{
                #(#z),*
            }
            impl<Target: #name + ?Sized> #root::Traverse<Target> for #data<Target>{
                fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Target::ExternRef> + 'a>{
                    return #{
                        let x = sfields.iter().map(|a|quote!{#root::Traverse::<Target>::traverse(&self.#a)});
                        quote!{
                            Box::new(::std::iter::empty()#(.chain(#x))*)
                        }
                    }
                }
                fn traverse_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Target::ExternRef> + 'a>{
                    return #{
                        let x = sfields.iter().map(|a|quote!{#root::Traverse::<Target>::traverse_mut(&mut self.#a)});
                        quote!{
                            Box::new(::std::iter::empty()#(.chain(#x))*)
                        }
                    }
                }
            }
            pub trait #name: #{opts.fp()}::CtxSpec<ExternRef = Self::_ExternRef> #{if opts.flags.contains(Flags::ASYNC){
                quote! {+ Send + Sync}
            }else{
                quote! {}
            }} #{if opts.flags.contains(Flags::WASIX){
                quote!{+ #root::wasix::XSpec}
            }else{
                quote! {}
            }}{
                type _ExternRef: Clone #{if opts.flags.contains(Flags::PIT){
                    quote!{+ From<#root::Pit<Vec<#{opts.fp()}::Value<Self>>>> + TryInto<#root::Pit<Vec<#{opts.fp()}::Value<Self>>>>}
                }else{
                    quote!{}
                }};
                fn data(&mut self) -> &mut #data<Self>;
                #(#fs)*

            }
            pub fn init<C: #name + 'static>(ctx: &mut C) -> #root::_rexport::anyhow::Result<()>{
                #(#init);*;
                return Ok(())
            }
            impl<Target: #name + ?Sized> Default for #data<Target>{
                fn default() -> Self{
                    Self{
                        #(#defaults),*
                    }
                }
            }
            impl<Target: #name + ?Sized> Clone for #data<Target>{
                fn clone(&self) -> Self{
                    Self{
                        #(#clones),*
                    }
                }
            }
        }
        use #internal_path::{#name,#data};
    }
}
