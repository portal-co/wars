use quote::format_ident;
use syn::Ident;
use waffle::{entity::EntityRef, Block, Func, Module};

pub fn mangle(m: &Module, f: Func, mut b: Block) -> Ident {
    if let Some(d) = m.funcs[f].body() {
        if d.entry == b {
            b = Block::invalid()
        }
    }
    format_ident!("{f}_{b}")
}

pub struct Compiler<'a> {
    pub module: &'a Module<'static>,
}
