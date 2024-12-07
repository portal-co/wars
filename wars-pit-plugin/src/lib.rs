use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    f32::consts::E,
    iter::once,
    sync::{Arc, Mutex, OnceLock},
};
use wars::*;

use pit_core::{Arg, Interface};
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
#[derive(Default)]
pub struct PitPlugin {
    pub tpit: OnceLock<BTreeSet<pit_core::Interface>>,
    pub extra: Vec<Arc<dyn PitPluginPlugin>>,
}

pub trait PitPluginPlugin {
    fn pre(&self, pit: &PitPlugin, module: &mut Module<'static>) -> anyhow::Result<()> {
        return Ok(());
    }
    fn choose_type(&self, opts: &Opts<Module<'static>>) -> anyhow::Result<Option<TokenStream>>;
    fn emit_method(
        &self,
        opts: &Opts<Module<'static>>,
        idx: usize,
        i: &Interface,
        s: &str,
        value: TokenStream,
        params: &[TokenStream],
    ) -> anyhow::Result<TokenStream>;
    fn emit_drop(
        &self,
        opts: &Opts<Module<'static>>,
        idx: usize,
        value: TokenStream,
    ) -> anyhow::Result<TokenStream> {
        return Ok(quasiquote!(#{opts.fp()}::ret(Ok(()))));
    }
    fn post(
        &self,
        parent: &PitPlugin,
        idx: usize,
        opts: &Opts<Module<'static>>,
    ) -> anyhow::Result<TokenStream>;
}

impl PitPlugin {
    pub fn tpit(&self, opts: &Opts<Module<'static>>) -> &BTreeSet<Interface> {
        return self.tpit.get_or_init(|| {
            pit_patch::get_interfaces(&opts.module)
                .unwrap()
                .into_iter()
                .collect()
        });
    }
    pub fn host_tpit(&self, opts: &Opts<Module<'static>>) -> anyhow::Result<TokenStream> {
        let mut a;
        let root = &opts.crate_path;
        a = quote! {#root::Infallible};
        for e in self.extra.iter() {
            if let Some(c) = e.choose_type(opts)? {
                a = quote! {
                    #root::Either<#c,#a>
                };
            }
        }
        return Ok(a);
    }
    pub fn apply_host(
        &self,
        mut x: TokenStream,
        opts: &Opts<Module<'static>>,
        i: &Interface,
        s: &str,
        params: &[TokenStream],
    ) -> anyhow::Result<TokenStream> {
        let root = &opts.crate_path;
        for (idx, e) in self.extra.iter().enumerate() {
            if let Some(c) = e.choose_type(opts)? {
                x = quasiquote! {
                    match host{
                        #root::Either::Right(host) => #x,
                        #root::Either::Left(host) => #{e.emit_method(opts,idx, i, s, quote! {host}, params)?},
                    }
                };
            }
        }
        return Ok(x);
    }
    pub fn apply_drop(
        &self,
        mut x: TokenStream,
        opts: &Opts<Module<'static>>,
    ) -> anyhow::Result<TokenStream> {
        let root = &opts.crate_path;
        for (idx, e) in self.extra.iter().enumerate() {
            if let Some(c) = e.choose_type(opts)? {
                x = quasiquote! {
                    match host{
                        #root::Either::Right(host) => #x,
                        #root::Either::Left(host) => #{e.emit_drop(opts,idx, quote! {host})?},
                    }
                };
            }
        }
        return Ok(x);
    }
    pub fn wrap(
        &self,
        opts: &Opts<Module<'static>>,
        mut x: TokenStream,
    ) -> anyhow::Result<TokenStream> {
        let root = &opts.crate_path;
        for e in self.extra.iter() {
            if let Some(c) = e.choose_type(opts)? {
                x = quasiquote! {
                    #root::Either::Right(#x)
                };
            }
        }
        return Ok(x);
    }
}
impl Plugin for PitPlugin {
    fn exref_bounds(&self, opts: &Opts<Module<'static>>) -> anyhow::Result<Option<TokenStream>> {
        let root = &opts.crate_path;
        Ok(Some(
            quasiquote! {From<#root::Pit<Vec<#{opts.fp()}::Value<Self>>,#{self.host_tpit(opts)?}>> + TryInto<#root::Pit<Vec<#{opts.fp()}::Value<Self>>,#{self.host_tpit(opts)?}>>},
        ))
    }
    fn pre(&self, module: &mut Module<'static>) -> anyhow::Result<()> {
        for p in self.extra.iter() {
            p.pre(self, module)?;
        }
        Ok(())
    }

    fn import(
        &self,
        opts: &Opts<Module<'static>>,
        module: &str,
        name: &str,
        params: Vec<TokenStream>,
    ) -> anyhow::Result<Option<TokenStream>> {
        let root = &opts.crate_path;
        let mut params = params.into_iter();
        if let Some(i) = module.strip_prefix("pit/") {
            let x: [u8; 32] = hex::decode(i).unwrap().try_into().unwrap();
            if let Some(s) = name.strip_prefix("~") {
                let s = {
                    let mut h = sha3::Sha3_256::default();
                    h.update(s);
                    h.finalize()
                };
                return Ok(Some(quasiquote! {
                    #{opts.fp()}::ret(Ok(#{opts.fp()}::Value::<C>::ExternRef(#root::Pit::Guest{
                        id: [#(#x),*],
                        x: #{opts.fp()}::CoeVec::coe(#root::tuple_list::tuple_list!(#(#params),*)),
                        s: [#(#s),*],
                    }.into())))
                }));
            }

            let mut f = params.next().unwrap();
            // let id = format_ident!("{}", bindname(&format!("pit/{i}/~{PIT_NS}/{name}")));
            // ctx.#id(x.x,#(#params),*)
            let params = params.collect::<Vec<_>>();
            let cases = opts
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
                            let mut y = #{opts.fp()}::CoeVec::coe(#root::tuple_list::tuple_list!(#(#params),*));
                            y.extend(&mut x.clone());
                            ctx.#id(#{opts.fp()}::CoeVec::uncoe(y))
                        }
                    }
                });
            let interface = self.tpit(opts).iter().find(|a| a.rid() == x);
            let meth = interface.and_then(|a| a.methods.get(name));
            return Ok(Some(quasiquote! {
                'a: {
                    let x = #f;
                    let #{opts.fp()}::Value::<C>::ExternRef(x) = x else{
                        break 'a #{opts.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("not an externref")))
                    };
                    let Ok(x) = x.try_into() else{
                        break 'a #{opts.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("not a pit externref")))
                    };
                    match x{
                        #root::Pit::Guest{s,x,id} => match s{
                            #(#cases),*,
                            _ => break 'a #{opts.fp()}::ret(Err(#root::_rexport::anyhow::anyhow!("invalid target")))
                        },
                        #root::Pit::Host{host} => #{let t = quote!{match host{}};self.apply_host(t,opts,interface.unwrap(),name,&params)?}
            _ => todo!()
                    }
                }
            }));
        }
        if module == "pit" && name == "drop" {
            let mut f = params.next().unwrap();
            let cases = opts
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
                        ([#(#x),*],[#(#s),*]) => ctx.#id(#{opts.fp()}::CoeVec::uncoe(x))
                    )
                });
            return Ok(Some(quasiquote! {
                'a: {
                    let x = #f;
                    let #{opts.fp()}::Value::<C>::ExternRef(x) = x else{
                        break 'a #{opts.fp()}::ret(Ok(()));
                    };
                    if let Ok(x) = x.try_into(){
                        match x{
                            #root::Pit::Guest{s,x,id} => => break 'a match (id,s){
                                #(#cases),*,
                                _ => #{opts.fp()}::ret(Ok(()))
                            },
                            #root::Pit::Host{host} => break 'a #{self.apply_drop(quasiquote!(#{opts.fp()}::ret(Ok(()))),opts)?}
                        }
                    }else{
                        break 'a #{opts.fp()}::ret(Ok(()))
                    }
                }
            }));
        };
        return Ok(None);
    }

    fn post(&self, opts: &Opts<Module<'static>>) -> anyhow::Result<TokenStream> {
        let root = &opts.crate_path;
        let name = opts.name.clone();

        let bs = self
            .extra
            .iter()
            .enumerate()
            .map(|(i, x)| x.post(self, i, opts))
            .collect::<anyhow::Result<Vec<_>>>()?;
        return Ok(quote! {
            #(#bs)*
        });
    }
}
pub fn indexed_lookup(root: &TokenStream, i: usize, a: TokenStream) -> TokenStream {
    if i == 0 {
        return quote! {#root::Either::Right(#a)};
    }
    return quasiquote!(#root::Either::Left(#{indexed_lookup(root, i - 1, a)}));
}
pub struct Passthrough {}
impl PitPluginPlugin for Passthrough {
    fn choose_type(&self, opts: &Opts<Module<'static>>) -> anyhow::Result<Option<TokenStream>> {
        Ok(Some(quasiquote!(
            #{opts.host_tpit()}
        )))
    }

    fn emit_method(
        &self,
        opts: &Opts<Module<'static>>,
        idx: usize,
        i: &Interface,
        s: &str,
        value: TokenStream,
        params: &[TokenStream],
    ) -> anyhow::Result<TokenStream> {
        let meth = i.methods.get(s);
        let i2 = i.rid_str();
        let root = &opts.crate_path;
        Ok(match opts.roots.get("tpit_rt") {
            None => quote! {
                match #value{

                }
            },
            Some(r) => quasiquote! {
                    match #value{
                        host => {
                    let casted = unsafe{
                        host.cast::<Box<dyn #{format_ident!("R{}",i2)}>>()
                    };
                    let a = casted.#{format_ident!("{s}")}(#{
                        let p = params.iter().zip(meth.unwrap().params.iter()).map(|(x,y)|match y{
                            Arg::Resource { ty, nullable, take, ann } => quasiquote!{
                                Box::new(Shim{wrapped: ctx, x: #x}).into()
                            },
                            _ => quote!{
                                #x
                            }
                        });

                        quote!{
                            #(#p),*
                        }
                    });
                    break 'a #{opts.fp()}::ret(Ok(#root::tuple_list::tuple_list!(#{
                        let r = meth.unwrap().rets.iter().enumerate().map(|(i,r)|{
                            let i = syn::Index{index: i as u32, span: Span::call_site()};
                            let i = quote!{
                                a.#i
                            };
                            match r{
                                Arg::Resource { ty, nullable, take, ann } => quote!{
                                    #{opts.fp()}::Value::<C>::ExternRef(#root::Pit::Host{host: #{indexed_lookup(root,idx,quasiquote!{
                                        unsafe{i.cast()}
                                    })}})
                                },
                                _ => i
                            }
                        });

                        quote!{
                            #(#r),*
                        }
                    })));
                }
            }
                },
        })
    }

    fn post(
        &self,
        parent: &PitPlugin,
        idx: usize,
        opts: &Opts<Module<'static>>,
    ) -> anyhow::Result<TokenStream> {
        let root = &opts.crate_path;
        let name = opts.name.clone();
        let a = match opts.roots.get("tpit_rt") {
            None => quote! {},
            Some(tpit_rt) => quasiquote! {
                impl<T: #name + ?Sized> Into<#tpit_rt::Tpit<()>> for Box<Shim<T>>{
                    fn into(self) -> #tpit_rt::Tpit<()>{
                        if let #{opts.fp()}::Value::<T>::ExternRef(e) = *self{
                            if let Ok(a) = e.try_into(){
                                if let #root::Pit::Host{host} = a{
                                    return host;
                                }
                            }
                        }
                        Default::default()
                    }
                }
                impl<T: #name + ?Sized> Drop for Shim<T>{
                    fn drop(&mut self){
                        let ctx = unsafe{
                            &mut *self.wrapped
                        };
                        #root::rexport::tramp::tramp(#{opts.import("pit","drop",once(quote!{
                            self.x.clone()
                        }))?})
                    }
                }
                #{
                    let a = parent.tpit(opts).iter().map(|i|{
                        let tname = format_ident!("R{}",i.rid_str());
                        let meths = i.methods.iter().map(|(a,b)|
                            Ok(quasiquote!{
                                fn #{format_ident!("{a}")}#{pit_rust_guest::render_sig(&pit_rust_guest::Opts { root: tpit_rt.clone(), salt: vec![], tpit: true },&tpit_rt.clone(),i,b,&quote! {&mut self},false)}{
                                    let ctx = unsafe{
                                        &mut *self.wrapped
                                    };
                                    let res = #{opts.import(&format!("pit/{}",i.rid_str()),&format!("{a}"),once(Ok(quote!{self.x.clone()})).chain(b.params.iter().enumerate().map(|(i,p)|{
                                        let i = format_ident!("p{i}");
                                        Ok(match p{
                                            Arg::Resource{ty,nullable,take,ann} => {
                                                quote!{
                                                    #{opts.fp()}::Value::<C>::ExternRef(Pit::Host{host:#{indexed_lookup(opts,i,dxquasiquote!{
                                                        unsafe{
                                                            #i.cast()
                                                        }
                                                    })?}}.into())
                                                }
                                            }
                                            _ => quote!{
                                                #i
                                            }
                                        })
                                    })).collect::<anyhow::Result<Vec<_>>>()?.into_iter())?};
                                    let res = #root::rexport::tramp::tramp(res).unwrap().into_tuple()
                                    ;
                                    #{                                        let r = b.rets.iter().enumerate().map(|(i,r)|{
                                        let i = syn::Index{index: i as u32, span: Span::call_site()};
                                        let i = quote!{
                                            res.#i
                                        };
                                        match r{
                                            Arg::Resource { ty, nullable, take, ann } => quote!{
                                                Box::new(Shim{wrapped:self.wrapped,x: #i}).into()
                                            },
                                            _ => i
                                        }
                                    });

                                    quote!{
                                        #(#r),*
                                    }}
                                }
                        })).collect::<anyhow::Result<Vec<_>>>()?;
                        Ok(quote!{
                            impl<C: #name + ?Sized> #tname for Shim<C>{
                                #(#meths),*
                            }
                        })
                    }).collect::<anyhow::Result<Vec<_>>>()?;
                    quote!{
                        #(#a)*
                    }
                }
            },
        };
        Ok(a)
    }
}
