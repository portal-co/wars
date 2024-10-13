use crate::*;
use std::{
    borrow::Cow, collections::{BTreeMap, BTreeSet}, convert::Infallible, f32::consts::E, iter::once, sync::{Arc, OnceLock}
};


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
pub struct PitPlugin{
    pub tpit: OnceLock<BTreeSet<pit_core::Interface>>
}

impl PitPlugin{
    pub fn tpit(&self, opts: &Opts<Module<'static>>) -> &BTreeSet<Interface>{
        return self.tpit.get_or_init(||pit_patch::get_interfaces(&opts.module).unwrap().into_iter().collect())
    }
}
impl Plugin for PitPlugin{
    fn exref_bounds(&self, opts: &Opts<Module<'static>>) -> Option<TokenStream> {
        let root = &opts.crate_path;
        Some(quasiquote!{From<#root::Pit<Vec<#{opts.fp()}::Value<Self>>,#{opts.host_tpit()}>> + TryInto<#root::Pit<Vec<#{opts.fp()}::Value<Self>>,#{opts.host_tpit()}>>})
    }
    fn pre(&self, module: &mut Module<'static>) {
        
    }

    fn import(&self, opts: &Opts<Module<'static>>, module: &str, name: &str, params: Vec<TokenStream>) -> Option<TokenStream> {
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
                return Some(quasiquote! {
                    #{opts.fp()}::ret(Ok(#{opts.fp()}::Value::<C>::ExternRef(#root::Pit::Guest{
                        id: [#(#x),*],
                        x: #{opts.fp()}::CoeVec::coe(#root::tuple_list::tuple_list!(#(#params),*)),
                        s: [#(#s),*],
                    }.into())))
                });
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
                let interface = self.tpit(opts).iter().find(|a|a.rid() == x);
                let meth = interface.and_then(|a|a.methods.get(name));
            return Some(quasiquote! {
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
                        #root::Pit::Host{host} => #{match opts.roots.get("tpit_rt"){
                            None => quote!{
                                match host{

                                }
                            },
                            Some(r) => quasiquote!{
                                let casted = unsafe{
                                    host.cast::<Box<dyn #{format_ident!("R{}",i)}>>()
                                };
                                let a = casted.#{format_ident!("{name}")}(#{
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
                                                #{opts.fp()}::Value::<C>::ExternRef(#root::Pit::Host{host: unsafe{i.cast()}})
                                            },
                                            _ => i
                                        }
                                    });

                                    quote!{
                                        #(#r),*
                                    }
                                })));
                            }
                        }}
            _ => todo!()
                    }
                }
            });
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
            return Some(quasiquote! {
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
                            #root::Pit::Host{host} => break 'a #{opts.fp()}::ret(Ok(()))
                        }
                    }else{
                        break 'a #{opts.fp()}::ret(Ok(()))
                    }
                }
            });
        };
        return None;
    }

    fn post(&self, opts: &Opts<Module<'static>>) -> TokenStream {
        let root = &opts.crate_path;
        let name = opts.name.clone();
        match opts.roots.get("tpit_rt"){
            None => quote!{

            },
            Some(tpit_rt) => quasiquote!{
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
                        }))})
                    }
                }
                #{
                    let a = self.tpit(opts).iter().map(|i|{
                        let tname = format_ident!("R{}",i.rid_str());
                        let meths = i.methods.iter().map(|(a,b)|
                            quasiquote!{
                                fn #{format_ident!("{a}")}#{pit_rust_guest::render_sig(&pit_rust_guest::Opts { root: tpit_rt.clone(), salt: vec![], tpit: true },&tpit_rt.clone(),i,b,&quote! {&mut self},false)}{
                                    let ctx = unsafe{
                                        &mut 8self.wrappedPit::Host{host} = a{
                                            return host;
                                    };
                                    let res = #{opts.import(&format!("pit/{}",i.rid_str()),&format!("{a}"),once(quote!{self.x.clone()}).chain(b.params.iter().enumerate().map(|(i,p)|{
                                        let i = format_ident!("p{i}");
                                        match p{
                                            Arg::Resource{ty,nullable,take,ann} => {
                                                quote!{
                                                    #{opts.fp()}::Value::<C>::ExternRef(Pit::Host{host:unsafe{
                                                        #i.cast()
                                                    }}.into())
                                                }
                                            }
                                            _ => quote!{
                                                #i
                                            }
                                        }
                                    })))};
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
                            }
                        });
                        quote!{
                            impl<C: #name + ?Sized> #tname for Shim<C>{
                                #(#meths),*
                            }
                        }
                    });
                    quote!{
                        #(#a)*
                    }
                }
            }
        }
    }
}