use std::{collections::BTreeMap, sync::Arc};

use expander::{Edition, Expander};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Ident, LitBool, Path, Token};
use wars::{Flags, Opts};

struct O {
    pub crate_path: syn::Path,
    pub module: Vec<u8>,
    pub name: Ident,
    pub flags: Flags,
    pub embed: proc_macro2::TokenStream,
    pub data: BTreeMap<Ident, proc_macro2::TokenStream>, // pub cfg: Arc<dyn ImportCfg>,
    pub roots: BTreeMap<String, proc_macro2::TokenStream>,
}
// struct NoopCfg {}
// impl ImportCfg for NoopCfg {
//     fn import(&self, module: &str, name: &str) -> proc_macro2::TokenStream {
//         quote! {
//             compile_error!("import used")
//         }
//     }
// }
impl Parse for O {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut o = O {
            crate_path: syn::parse(quote! {::wars_rt}.into())?,
            module: vec![],
            name: Ident::new("Wars", Span::call_site()),
            flags: Default::default(),
            embed: Default::default(),
            data: BTreeMap::new(),
            roots: BTreeMap::new(),
            // cfg: Arc::new(NoopCfg {}),
        };
        while input.lookahead1().peek(Ident) {
            let i: Ident = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            match i.to_string().as_str() {
                "crate_path" => {
                    let s: syn::LitStr = input.parse()?;
                    o.crate_path = s.parse()?;
                }
                "env" => {
                    let s: syn::LitStr = input.parse()?;
                    let sp = s.span();
                    let s = std::env::var(s.value()).map_err(|x| syn::Error::new(sp, x))?;
                    let x = std::fs::read(s).map_err(|x| syn::Error::new(sp, x))?;
                    o.module = x;
                }
                "file" => {
                    let s: syn::LitStr = input.parse()?;
                    let sp = s.span();
                    let x = std::fs::read(s.value()).map_err(|x| syn::Error::new(sp, x))?;
                    o.module = x;
                }
                "inline" => {
                    let s: syn::LitStr = input.parse()?;
                    let sp = s.span();
                    let x = wat::parse_str(&s.value()).map_err(|x| syn::Error::new(sp, x))?;
                    o.module = x;
                }
                "name" => {
                    o.name = input.parse()?;
                }
                "r#async" => {
                    let b: LitBool = input.parse()?;
                    if b.value {
                        o.flags |= Flags::ASYNC
                    } else {
                        o.flags &= Flags::ASYNC.complement();
                    }
                }
                "legacy_host" => {
                    let b: LitBool = input.parse()?;
                    if b.value {
                        o.flags |= Flags::LEGACY
                    } else {
                        o.flags &= Flags::LEGACY.complement();
                    }
                }
                _ => {
                    match i
                        .to_string()
                        .as_str()
                        .strip_suffix("_path")
                        .map(|a| a.to_owned())
                    {
                        None => return Err(syn::Error::new(i.span(), "unexpected type")),
                        Some(a) => {
                            let s: syn::LitStr = input.parse()?;
                            o.roots.insert(a, s.parse()?);
                        }
                    }
                }
            };
            let _comma: Token![,] = input.parse()?;
        }
        // while input.lookahead1().peek(Token![;]){
        //     let _semi: Token![;] = input.parse()?;
        // }
        Ok(o)
    }
}

#[proc_macro]
pub fn wars(a: TokenStream) -> TokenStream {
    let o = parse_macro_input!(a as O);
    let x = wars::go(
        &Opts {
            crate_path: o.crate_path,
            module: o.module,
            name: o.name,
            flags: o.flags,
            embed: o.embed,
            data: o.data,
            roots: o.roots,
            // cfg: o.cfg,
        }
        .to_mod(),
    );
    let expanded = Expander::new("wars")
        .add_comment("This is generated code!".to_owned())
        .fmt(Edition::_2021)
        .verbose(true)
        // common way of gating this, by making it part of the default feature set
        .dry(cfg!(feature = "no-file-expansion"))
        .write_to_out_dir(x.clone())
        .unwrap_or_else(|e| {
            eprintln!("Failed to write to file: {:?}", e);
            x
        });
    expanded.into()
}

// #[proc_macro]
// pub fn wasix(a: TokenStream) -> TokenStream {
//     let x = wars::wasix::wasix();
//     let expanded = Expander::new("wars_wasix")
//         .add_comment("This is generated code!".to_owned())
//         .fmt(Edition::_2021)
//         .verbose(true)
//         // common way of gating this, by making it part of the default feature set
//         .dry(cfg!(feature = "no-file-expansion"))
//         .write_to_out_dir(x.clone())
//         .unwrap_or_else(|e| {
//             eprintln!("Failed to write to file: {:?}", e);
//             x
//         });
//     expanded.into()
// }
