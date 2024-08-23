extern crate proc_macro;
use std::{collections::BTreeMap, fmt::Display};

use expander::{Edition, Expander};
// use litrs::StringLit;
use proc_macro::TokenStream;
use proc_macro2::Ident;
use syn::{parse::Parse, parse_macro_input, LitStr, Token};
use legacy_wars_core::{Opts,z};
fn to_compile_error(msg: impl Display) -> TokenStream {
    let msg = format!("{msg}");
    return quote::quote! { compile_error!(#msg) }.into();
}


#[proc_macro]
pub fn wars(input: TokenStream) -> TokenStream {
    // if not, a relevant error message will be generated.
    let input = parse_macro_input!(input as z);

    // get value of the string literal.
    let str_value = input.l.value();
    let f = std::fs::read(str_value);
    let f = match f {
        Err(e) => return to_compile_error(e),
        Ok(lit) => lit,
    };
    let t = legacy_wars_core::lower(
        &f,
        input.b,
        &input
            .n
            .clone()
            .map(|a| Opts {
                r#async: a.value().contains("a"),
                result: a.value().contains("r"),
                serde: a.value().contains("s"),

                imports: BTreeMap::new(),
                inherit: input.c.clone(),
                r#impl: false,
            })
            .unwrap_or(Opts {
                r#async: false,
                result: false,
                imports: BTreeMap::new(),
                inherit: input.c,
                serde: false,
                r#impl: false,
            }),
    );
    let t = match t {
        Ok(t) => t,
        Err(e) => return to_compile_error(e),
    };
    eprintln!("done");
    let expanded = Expander::new("wasm")
        .add_comment("This is generated code!".to_owned())
        .fmt(Edition::_2021)
        .verbose(true)
        // common way of gating this, by making it part of the default feature set
        .dry(false)
        .write_to_out_dir(t.clone())
        .unwrap_or_else(|e| {
            eprintln!("Failed to write to file: {:?}", e);
            t
        });
    // eprintln!("{}",t);
    return expanded.into();
}

#[cfg(test)]
mod tests {
    use super::*;
}
