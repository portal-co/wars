use std::{collections::BTreeMap, path::PathBuf, str::FromStr};

// use more_waffle::*;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
// use sha3::Digest;
use syn::Index;
use syn::{parse::Parse, parse_macro_input, LitStr, Token};
use waffle::{
    entity::EntityRef, Block, BlockTarget, ExportKind, Func, FunctionBody, ImportKind, MemoryArg,
    Module, Signature, SignatureData, Value,
};
pub mod tcore;
pub mod fcopy;

#[derive(Clone)]
pub struct Opts {
    pub r#async: bool,
    pub result: bool,
    pub imports: BTreeMap<(String, String), proc_macro2::TokenStream>,
    pub inherit: Ident,
    pub serde: bool,
    pub r#impl: bool,
}
impl Opts {
    fn needs_tco(&self) -> bool {
        return self.r#impl;
    }
    fn ref_type(&self) -> TokenStream{
        if self.r#impl{
            quote!{}
        }else{
            quote!{&mut }
        }
    }
}
macro_rules! binop {
    ($bl:tt,$vn:tt, $ra:tt, $rb:tt, $args:tt, $op:tt) => {{
        let ra = $args[0];
        let $ra = Ident::new(&ra.to_string(), Span::call_site());
        let rb = $args[1];
        let $rb = Ident::new(&rb.to_string(), Span::call_site());
        $bl = quote! {
            #$bl;
            let #$vn = $op;
        }
    }};
}
macro_rules! ternop {
    ($bl:tt,$vn:tt, $ra:tt, $rb:tt,$rc:tt, $args:tt, $op:tt) => {{
        let ra = $args[0];
        let $ra = Ident::new(&ra.to_string(), Span::call_site());
        let rb = $args[1];
        let $rb = Ident::new(&rb.to_string(), Span::call_site());
        let rb = $args[2];
        let $rc = Ident::new(&rb.to_string(), Span::call_site());
        $bl = quote! {
            #$bl;
            let #$vn = $op;
        }
    }};
}
macro_rules! unop {
    ($bl:tt,$vn:tt, $ra:tt, $args:tt, $op:tt) => {{
        let ra = $args[0];
        let $ra = Ident::new(&ra.to_string(), Span::call_site());
        $bl = quote! {
            #$bl;
            let #$vn = $op;
        }
    }};
}
pub fn lower(v: &[u8], b: Ident, o: &Opts) -> anyhow::Result<proc_macro2::TokenStream> {
    // let v = bulk_memory_lowering::lower_bulk_memory(v)?;
    // let v = tcore::trampoline_bytes(&v)?;
    // let v = tcore::tcore_bytes(&v)?;
    return lower_module(Module::from_wasm_bytes(&v, &Default::default())?, b, o);
}
pub fn render_sig(
    m: &Module,
    n: SignatureData,
    opts: &Opts,
    ident: Ident,
    f: Func,
    force: bool,
) -> proc_macro2::TokenStream {
    let mut p = vec![];
    let mut r = vec![];
    if opts.needs_tco() && !force {
        let s = Ident::new(&format!("{ident}{f}State"), ident.span());
        p.push(quote! {
            mut state: #s
        })
    } else {
        for (pi, pr) in n.params.iter().enumerate() {
            let i = Ident::new(
                &pr.to_string().replace("i", "u").to_lowercase(),
                Span::call_site(),
            );
            let pi = Ident::new(&format!("p{pi}"), Span::call_site());
            p.push(quote! {
                #pi : #i
            })
        }
    }
    for pr in n.returns {
        r.push(Ident::new(
            &pr.to_string().replace("i", "u").to_lowercase(),
            Span::call_site(),
        ))
    }
    let mut s = quote! {
        (#(#r),*,)
    };
    if r.len() == 0 {
        s = quote! {()}
    };
    if opts.r#impl {
        s = quote! {
            (#s,impl #ident)
        }
    }
    if opts.result {
        s = quote! {::anyhow::Result<#s>};
    }
    let a = opts.ref_type();
    return quote! {
        (#a self, #(#p),*) -> #s
    };
}
pub fn new_state_enum(
    f: Option<&FunctionBody>,
    name: Func,
    ident: Ident,
    m: &Module,
) -> proc_macro2::TokenStream {
    let mut c = vec![];
    match f {
        Some(f) => {
            for (b, d) in f.blocks.entries() {
                let b = Ident::new(&b.to_string(), Span::call_site());
                let mut p = vec![];
                for pr in d.params.iter() {
                    p.push(Ident::new(
                        &pr.0.to_string().replace("i", "u").to_lowercase(),
                        Span::call_site(),
                    ))
                }
                c.push(quote! {
                    #b(#(#p),*)
                })
            }
        }
        None => {
            let mut p = vec![];
            for pr in m.signatures[m.funcs[name].sig()].params.iter() {
                p.push(Ident::new(
                    &pr.to_string().replace("i", "u").to_lowercase(),
                    Span::call_site(),
                ))
            }
            c.push(quote! {
                Of(#(#p),*)
            })
        }
    }
    let s = Ident::new(&format!("{ident}{name}State"), ident.span());
    return quote! {
        enum #s{
            #(#c),*
        }
    };
}
pub fn out_dir() -> PathBuf {
    // First we get the arguments for the rustc invocation
    let mut args = std::env::args();

    // Then we loop through them all, and find the value of "out-dir"
    let mut out_dir = None;
    while let Some(arg) = args.next() {
        if arg == "--out-dir" {
            out_dir = args.next();
        }
    }

    // Finally we clean out_dir by removing all trailing directories, until it ends with target
    let mut out_dir = PathBuf::from(out_dir.expect("Failed to find out_dir"));
    while !out_dir.ends_with("target") {
        if !out_dir.pop() {
            // We ran out of directories...
            panic!("Failed to find out_dir");
        }
    }

    out_dir
}
pub fn lower_module(
    mut m: Module,
    ident: Ident,
    opts: &Opts,
) -> anyhow::Result<proc_macro2::TokenStream> {
    m.expand_all_funcs()?;
    tcore::trampoline_module(&mut m, true)?;
    tcore::tcore(&mut m, true)?;
    //::tcore::tcore(&mut m,false)?;
    eprintln!("lowering");
    let mut fs: Vec<proc_macro2::TokenStream> = vec![];
    let mut imps = BTreeMap::new();
    for i in m.imports.clone() {
        if let ImportKind::Func(f) = i.kind {
            imps.insert(f, (i.module.clone(), i.name.clone()));
        }
    }
    let mut z = vec![];
    let zn = Ident::new(&format!("{ident}Data"), Span::call_site());
    let mut init = quote! {
        let this = self;
    };
    fs.push(quote! {
        fn z(&mut self) -> &mut #zn;
    });
    // eprintln!("before globals");
    for (g, d) in m.globals.clone().entries() {
        let n = Ident::new(&g.to_string(), Span::call_site());
        let t = Ident::new(
            &d.ty.to_string().to_lowercase().replace("i", "u"),
            Span::call_site(),
        );
        z.push(quote! {
            #n : #t
        });
        fs.push(quote! {
            fn #n(&mut self) -> &mut #t{
                return &mut self.z().#n;
            }
        });
        if let Some(v) = d.value.clone() {
            init = quote! {
                #init;
                *this.#n() = (#v as #t);
            }
        }
    }
    // eprintln!("before memories");
    for (m, d) in m.memories.clone().entries() {
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

        let n = Ident::new(&m.to_string(), Span::call_site());
        z.push(quote! {
            #n : Vec<u8>
        });
        fs.push(quote! {
            fn #n(&mut self) -> &mut Vec<u8>{
                return &mut self.z().#n;
            }
        });
        let pk = d.initial_pages * 65536;
        init = quote! {
            let l = #pk.max(self.#n().len());
            self.#n().resize(l,0);
            #init;
        };
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
                init = quote! {
                    #init;
                    this.#n()[#o..#pk].copy_from_slice(&[#(#d),*])
                }
            }
        }
    }
    let tag = if opts.r#async {
        quote! {async}
    } else {
        quote! {}
    };
    let mut wrap = if opts.r#async {
        quote! {.await}
    } else {
        quote! {}
    };
    // let (tx,rx) = std::sync::mpsc::channel();
    // std::thread::scope(|sc|{
    // eprintln!("before funcs");
    let mut states = vec![];
    for f in m.funcs.iter() {
        // eprintln!("starting doing func");
        match m.funcs[f].clone() {
            waffle::FuncDecl::Import(s, _) => {
                let mut stat = new_state_enum(None, f, ident.clone(), &m);
                states.push(stat);
                let mut i = None;
                for im in m.imports.iter() {
                    if im.kind == ImportKind::Func(f) {
                        i = Some(im.clone());
                    }
                }
                let i = i.unwrap();
                let n = render_sig(&m, m.signatures[s].clone(), opts, ident.clone(), f, false);
                let x = format!("{}_{}", i.module, i.name);
                let x = Ident::new(&x, Span::call_site());
                let mn = mangle(&m, f, Block::invalid());
                let mut p = quote! {
                    self
                };
                for (pi, x) in m.signatures[s].clone().params.into_iter().enumerate() {
                    let pi = Ident::new(&format!("p{pi}"), Span::call_site());
                    p = quote! {
                        #p
                        ,#pi
                    }
                }
                let i = match opts.imports.get(&(i.module.clone(), i.name.clone())) {
                    None => quote! {
                        #tag fn #x #n;
                        #tag fn #mn #n{
                            return Self::#x(#p)#wrap
                        }
                    },
                    Some(t) => {
                        let l = quote! {
                            return (#t)(this,#p)#wrap;
                        };
                        quote! {
                            #tag fn #x #n{
                                #l
                            }
                            #tag fn #mn #n{
                                return Self::#x(#p)#wrap
                            }
                        }
                    }
                };
                fs.push(i);
            }
            waffle::FuncDecl::Lazy(_, _, _) => panic!("lazy function found"),
            waffle::FuncDecl::Body(s, _, mut b) => {
                // let m = &m;
                // let tx = tx.clone();
                // sc.spawn(move||{
                    b.optimize(&Default::default());
                b.convert_to_max_ssa(None);
                // eprintln!("doing func");
                let mut bs = vec![];
                let n = render_sig(&m, m.signatures[s].clone(), opts, ident.clone(), f, false);
                let x = mangle(&m, f, b.entry);
                let mut stat = new_state_enum(Some(&b), f, ident.clone(), &m);
                states.push(stat);
                // std::thread::scope(move|sc|{
                for k in b.blocks.entries().map(|a| a.0).collect::<Vec<_>>() {
                    // let b = b.clone();
                    // let m = &*m;
                    // let tx = tx.clone();
                    // sc.spawn(move||{

                    bs.push(lower_body(
                        &m,
                        f,
                        &b,
                        k,
                        wrap.clone(),
                        opts,
                        ident.clone(),
                        &mut states,
                    )?);
                    // });
                }
                let e = Ident::new(&b.entry.to_string(), Span::call_site());
                let mut p = vec![];
                for (i, _) in m.signatures[s].params.iter().enumerate() {
                    p.push(Ident::new(&format!("p{i}"), Span::call_site()));
                }
                let state = Ident::new(&format!("{ident}{f}State"),ident.span());
                if !opts.needs_tco() {
                    fs.push(quote! {
                        #tag fn #x #n{
                            let this = self;
                            let mut state = #state::#e(#(#p),*);
                            loop{
                                match state{
                                    #(#bs)*
                                }
                            }
                        }
                    });
                } else {
                    fs.push(quote! {
                        #tag fn #x #n{
                            let this = self;
                                match state{
                                    #(#bs)*
                                }
                        }
                    });
                }
                // eprintln!("did func");
                //     })
                // });
                // waffle::passes::resolve_aliases::run(&mut b);
            }
            waffle::FuncDecl::Compiled(_, _, _) => {
                panic!("not yet implemented: {}; {}", file!(), line!())
            }
            waffle::FuncDecl::None => panic!("not yet implemented: {}; {}", file!(), line!()),
        }
    }
    // });
    // drop(tx);
    // while let Ok(a) = rx.recv(){
    //     let a = a?;
    //     fs.push(TokenStream::from_str(&a).unwrap());
    // }
    for xp in m.exports.clone() {
        match &xp.kind {
            ExportKind::Func(f) => {
                let f = *f;
                let s = m.funcs[f].sig();
                let n = render_sig(&m, m.signatures[s].clone(), opts, ident.clone(), f, true);
                let x = mangle(
                    &m,
                    f,
                    match m.funcs[f].body() {
                        None => Block::invalid(),
                        Some(f) => f.entry,
                    },
                );
                let mn = Ident::new(&xp.name, Span::call_site());
                let mut p = vec![];
                for (pi, x) in m.signatures[s].clone().params.into_iter().enumerate() {
                    let pi = Ident::new(&format!("p{pi}"), Span::call_site());
                    p.push(pi);
                }
                let mut p = quote! {
                    #(#p),*
                };
                if opts.needs_tco() {
                    let e = match m.funcs[f].body() {
                        None => Ident::new("Of", Span::call_site()),
                        Some(f) => Ident::new(&f.entry.to_string(), Span::call_site()),
                    };
                    let f = Ident::new(&format!("{ident}{f}State"), ident.span());
                    p = quote! {
                        #f::#e(#p)
                    };
                }
                let i = quote! {
                    #tag fn #mn #n{
                        return self.#x(#p)#wrap;
                    }
                };
                fs.push(i);
            }
            ExportKind::Table(_) => {}
            ExportKind::Global(g) => {
                let d = m.globals[*g].clone();
                let t = Ident::new(
                    &d.ty.to_string().to_lowercase().replace("i", "u"),
                    Span::call_site(),
                );
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
    if let Some(s) = m.start_func {
        let s = Ident::new(&s.to_string(), Span::call_site());
        init = quote! {
            #init;
            return this.#s()#wrap
        }
    } else {
        if opts.result {
            init = quote! {
                #init;
                return Ok(())
            }
        }
    }
    let mut ptag = quote! {()};
    if opts.result {
        ptag = quote! {::anyhow::Result<#ptag>}
    }
    fs.push(quote! {
        #tag fn init(&mut self) -> #ptag{
            #init;
        }
    });
    let trait_marker = if opts.r#async {
        quote! {
            #[::async_trait::async_trait]
        }
    } else {
        quote! {}
    };
    let inherit = &opts.inherit;
    let mut extra_data = quote! {};
    if opts.serde {
        extra_data = quote! {
            #extra_data , ::serde::Serialize, ::serde::Deserialize
        }
    }
    return Ok(quote! {
        #(#states)*
        #trait_marker
        pub trait #ident: #inherit{
        #(#fs)*
        }
        #[derive(Default,Clone,Eq,Ord,PartialEq,PartialOrd #extra_data)]
        pub struct #zn{
            #(#z),*
        }
    });
}
pub fn mangle(m: &Module, f: Func, a: Block) -> Ident {
    let mut n = format!("internal_{f}");
    if let Some(d) = m.funcs[f].body() {
        if a != d.entry {
            n = format!("{n}_{a}");
        }
    }
    let n = Ident::new(&n, Span::call_site());
    return n;
}
pub struct z {
    pub l: LitStr,
    pub m: Token![=>],
    pub b: Ident,
    pub mc: Token![:],
    pub c: Ident,
    pub n: Option<LitStr>,
}
impl Parse for z {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        return Ok(z {
            l: input.parse()?,
            m: input.parse()?,
            b: input.parse()?,
            mc: input.parse()?,
            c: input.parse()?,
            n: input.parse()?,
        });
    }
}

pub fn lower_body(
    m: &Module,
    f: Func,
    b: &FunctionBody,
    a: Block,
    wrap: TokenStream,
    opts: &Opts,
    ident: Ident,
    states: &mut Vec<proc_macro2::TokenStream>,
) -> anyhow::Result<proc_macro2::TokenStream> {
    let bn = Ident::new(&a.to_string(), Span::call_site());
    let mut bp = vec![];
    for (i, _) in b.blocks[a].params.iter().enumerate() {
        bp.push(Ident::new(&format!("p{i}"), Span::call_site()));
    }
    let n = mangle(m, f, a);
    let s = render_sig(
        m,
        SignatureData {
            params: b.blocks[a].params.iter().map(|a| a.0).collect(),
            returns: b.rets.clone(),
        },
        opts,
        ident.clone(),
        f,
        false,
    );
    let mut bl = quote! {};
    for (pi, (t, v)) in b.blocks[a].params.iter().enumerate() {
        let pi = Ident::new(&format!("p{pi}"), Span::call_site());
        let v = format!("{v}");
        let v = Ident::new(&v, Span::call_site());
        bl = quote! {
            #bl;
            let #v = #pi
        }
    }
    let unr = if !opts.result {
        quote! {
            unreachable!()
        }
    } else {
        quote! {
            ::anyhow::bail!("unreachable code")
        }
    };
    for v in b.blocks[a].insts.iter() {
        let vn = Ident::new(&v.to_string(), Span::call_site());
        match b.values[*v].clone() {
            waffle::ValueDef::BlockParam(_, pi, _) => {
                let pi = Ident::new(&format!("p{pi}"), Span::call_site());
                let v = format!("{v}");
                let v = Ident::new(&v, Span::call_site());
                bl = quote! {
                    #bl;
                    let #v = #pi
                }
            }
            waffle::ValueDef::Operator(o, r, _) => {
                let args = b.arg_pool[r].to_vec();
                pub fn memory_(m: MemoryArg) -> proc_macro2::TokenStream {
                    let n = Ident::new(&m.memory.to_string(), Span::call_site());
                    let x = m.offset as usize;
                    return quote! {
                        (&mut (this.#n())[#x..])
                    };
                }
                pub fn memory_arg(m: MemoryArg, args: &[Value]) -> proc_macro2::TokenStream {
                    let m = memory_(m);
                    let o = args[0];
                    let o = Ident::new(&o.to_string(), Span::call_site());
                    let o = quote! {
                        (&mut (#m)[(#o as usize)..])
                    };
                    return o;
                }
                match o {
                    waffle::Operator::Unreachable => {
                        bl = quote! {
                            #bl;
                            #unr
                        }
                    }
                    waffle::Operator::Nop => {}
                    waffle::Operator::Call { function_index } => {
                        let mut a = Block::invalid();
                        if let Some(d) = m.funcs[function_index].body() {
                            a = d.entry;
                        }
                        let state =
                            Ident::new(&format!("{ident}{function_index}State"), ident.span());
                        let mi = mangle(m, function_index, a);
                        let args = args
                            .iter()
                            .map(|i| Ident::new(&i.to_string(), Span::call_site()))
                            .collect::<Vec<_>>();
                        let r = m.funcs[function_index].sig();
                        let r = m.signatures[r].clone();
                        let mut args = quote! {
                            #(#args),*
                        };
                        if opts.needs_tco() {
                            match m.funcs[function_index].body() {
                                None => {
                                    args = quote! {
                                        #state::Of(#args)
                                    }
                                }
                                Some(b) => {
                                    let b = b.entry;
                                    let b = Ident::new(&b.to_string(), Span::call_site());
                                    args = quote! {
                                        #state::#b(#args)
                                    }
                                }
                            }
                        };
                        bl = quote! {
                            #bl;
                            let #vn = this.#mi(#args)
                        };
                        if opts.r#async {
                            bl = quote! {
                                #bl;
                                let #vn = Box::pin(#vn).await;
                            };
                        }
                        if opts.result {
                            bl = quote! {
                                #bl;
                                let #vn = #vn?;
                            };
                        }
                        if opts.r#impl {
                            bl = quote! {
                                #bl;
                                let (#vn,this) = #vn;
                            }
                        }
                        // bl = quote!{
                        //     #bl;
                        //     let #vn = #vn #wrap;
                        // };
                        if r.returns.len() == 1 {
                            bl = quote! {
                                #bl;
                                let #vn = #vn.0
                            }
                        }
                    }
                    waffle::Operator::CallIndirect {
                        sig_index,
                        table_index,
                    } => panic!("not yet implemented: {}; {}", file!(), line!()),
                    waffle::Operator::Select => {
                        ternop!(bl,vn,ra,rb,rc,args,(if #rc != 0{#ra}else{#rb}))
                    }
                    waffle::Operator::TypedSelect { ty } => {
                        ternop!(bl,vn,ra,rb,rc,args,(if #rc != 0{#ra}else{#rb}))
                    }
                    waffle::Operator::GlobalGet { global_index } => {
                        let g = Ident::new(&global_index.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            let #vn = *this.#g()
                        }
                    }
                    waffle::Operator::GlobalSet { global_index } => {
                        let g = Ident::new(&global_index.to_string(), Span::call_site());
                        let r = Ident::new(&args[0].to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            *this.#g() = #r
                        }
                    }
                    waffle::Operator::I32Load { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u32::from_le_bytes((&#o[0..4]).try_into().unwrap())
                        };
                    }
                    waffle::Operator::I64Load { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u64::from_le_bytes((&#o[0..8]).try_into().unwrap())
                        };
                    }
                    waffle::Operator::F32Load { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = unsafe{std::mem::transmute::<_,f32>(u32::from_le_bytes((&#o[0..4]).try_into().unwrap()))}
                        };
                    }
                    waffle::Operator::F64Load { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = unsafe{std::mem::transmute::<_,f64>(u64::from_le_bytes((&#o[0..8]).try_into().unwrap()))}
                        };
                    }
                    waffle::Operator::I32Load8S { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = i8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i32;
                            let #vn = unsafe{
                                std::mem::transmute::<_,u32>(#vn)
                            }
                        };
                    }
                    waffle::Operator::I32Load8U { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u32
                        };
                    }
                    waffle::Operator::I32Load16S { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = i16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i32;
                            let #vn = unsafe{
                                std::mem::transmute::<_,u32>(#vn)
                            }
                        };
                    }
                    waffle::Operator::I32Load16U { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u32
                        };
                    }
                    waffle::Operator::I64Load8S { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = i8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                            let #vn = unsafe{
                                std::mem::transmute::<_,u64>(#vn)
                            }
                        };
                    }
                    waffle::Operator::I64Load8U { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u8::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                        };
                    }
                    waffle::Operator::I64Load16S { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = i16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                            let #vn = unsafe{
                                std::mem::transmute::<_,u64>(#vn)
                            }
                        };
                    }
                    waffle::Operator::I64Load16U { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u16::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                        };
                    }
                    waffle::Operator::I64Load32S { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = i32::from_le_bytes((&#o[0..4]).try_into().unwrap()) as i64;
                            let #vn = unsafe{
                                std::mem::transmute::<_,u64>(#vn)
                            }
                        };
                    }
                    waffle::Operator::I64Load32U { memory } => {
                        let o = memory_arg(memory, &args);
                        bl = quote! {
                            #bl;
                            let #vn = u32::from_le_bytes((&#o[0..4]).try_into().unwrap()) as u64
                        };
                    }
                    waffle::Operator::I32Store { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes(#n)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I64Store { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..8]).copy_from_slice(&(u64::to_le_bytes(#n)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::F32Store { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes(unsafe{
                                std::mem::transmute::<f32,u32>(#n)
                            })));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::F64Store { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..8]).copy_from_slice(&(u64::to_le_bytes(unsafe{
                                std::mem::transmute::<f64,u64>(#n)
                            })));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I32Store8 { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..1]).copy_from_slice(&(u8::to_le_bytes((#n & 0xff) as u8)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I32Store16 { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..2]).copy_from_slice(&(u16::to_le_bytes((#n & 0xffff) as u16)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I64Store8 { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..1]).copy_from_slice(&(u8::to_le_bytes((#n & 0xff) as u8)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I64Store16 { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..2]).copy_from_slice(&(u16::to_le_bytes((#n & 0xffff) as u16)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I64Store32 { memory } => {
                        let o = memory_arg(memory, &args);
                        let n = args[1];
                        let n = Ident::new(&n.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            (&mut #o[0..4]).copy_from_slice(&(u32::to_le_bytes((#n & 0xffffffff) as u32)));
                            let #vn = 0
                        };
                    }
                    waffle::Operator::I32Const { value } => {
                        bl = quote! {
                            #bl;
                            let #vn : u32 = #value;
                        }
                    }
                    waffle::Operator::I64Const { value } => {
                        bl = quote! {
                            #bl;
                            let #vn : u64 = #value;
                        }
                    }
                    waffle::Operator::F32Const { value } => {
                        bl = quote! {
                            #bl;
                            let #vn : f32 = unsafe{std::mem::transmute(#value)};
                        }
                    }
                    waffle::Operator::F64Const { value } => {
                        bl = quote! {
                            #bl;
                            let #vn : f64 = unsafe{std::mem::transmute(#value)};
                        }
                    }
                    waffle::Operator::I32Eqz => unop!(bl,vn,ra,args,((#ra == 0) as u32)),
                    waffle::Operator::I32Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
                    waffle::Operator::I32Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
                    waffle::Operator::I32LtS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i32>(#ra) < std::mem::transmute::<_,i32>(#rb)) as u32)
                    })),
                    waffle::Operator::I32LtU => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
                    waffle::Operator::I32GtS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i32>(#ra) > std::mem::transmute::<_,i32>(#rb)) as u32)
                    })),
                    waffle::Operator::I32GtU => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
                    waffle::Operator::I32LeS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i32>(#ra) <= std::mem::transmute::<_,i32>(#rb)) as u32)
                    })),
                    waffle::Operator::I32LeU => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
                    waffle::Operator::I32GeS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i32>(#ra) >= std::mem::transmute::<_,i32>(#rb)) as u32)
                    })),
                    waffle::Operator::I32GeU => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),
                    waffle::Operator::I64Eqz => unop!(bl,vn,ra,args,((#ra == 0) as u32)),
                    waffle::Operator::I64Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
                    waffle::Operator::I64Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
                    waffle::Operator::I64LtS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i64>(#ra) < std::mem::transmute::<_,i64>(#rb)) as u32)
                    })),
                    waffle::Operator::I64LtU => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
                    waffle::Operator::I64GtS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i64>(#ra) > std::mem::transmute::<_,i64>(#rb)) as u32)
                    })),
                    waffle::Operator::I64GtU => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
                    waffle::Operator::I64LeS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i64>(#ra) <= std::mem::transmute::<_,i64>(#rb)) as u32)
                    })),
                    waffle::Operator::I64LeU => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
                    waffle::Operator::I64GeS => binop!(bl,vn,ra,rb,args,(unsafe{
                        ((std::mem::transmute::<_,i64>(#ra) >= std::mem::transmute::<_,i64>(#rb)) as u32)
                    })),
                    waffle::Operator::I64GeU => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),
                    waffle::Operator::F32Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
                    waffle::Operator::F32Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
                    waffle::Operator::F32Lt => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
                    waffle::Operator::F32Gt => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
                    waffle::Operator::F32Le => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
                    waffle::Operator::F32Ge => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),

                    waffle::Operator::F64Eq => binop!(bl,vn,ra,rb,args,((#ra == #rb) as u32)),
                    waffle::Operator::F64Ne => binop!(bl,vn,ra,rb,args,((#ra != #rb) as u32)),
                    waffle::Operator::F64Lt => binop!(bl,vn,ra,rb,args,((#ra < #rb) as u32)),
                    waffle::Operator::F64Gt => binop!(bl,vn,ra,rb,args,((#ra > #rb) as u32)),
                    waffle::Operator::F64Le => binop!(bl,vn,ra,rb,args,((#ra <= #rb) as u32)),
                    waffle::Operator::F64Ge => binop!(bl,vn,ra,rb,args,((#ra >= #rb) as u32)),

                    waffle::Operator::I32Clz => unop!(bl,vn,ra,args,(#ra.leading_zeros())),
                    waffle::Operator::I32Ctz => unop!(bl,vn,ra,args,(#ra.trailing_zeros())),
                    waffle::Operator::I32Popcnt => unop!(bl,vn,ra,args,(#ra.count_ones())),
                    waffle::Operator::I32Add => binop!(bl,vn,ra,rb,args,(#ra.wrapping_add(#rb))),
                    waffle::Operator::I32Sub => binop!(bl,vn,ra,rb,args,(#ra.wrapping_sub(#rb))),
                    waffle::Operator::I32Mul => binop!(bl,vn,ra,rb,args,(#ra.wrapping_mul(#rb))),
                    waffle::Operator::I32DivS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) / std::mem::transmute::<_,i32>(#rb)))
                    })),
                    waffle::Operator::I32DivU => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
                    waffle::Operator::I32RemS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) % std::mem::transmute::<_,i32>(#rb)))
                    })),
                    waffle::Operator::I32RemU => binop!(bl,vn,ra,rb,args,(#ra % #rb)),
                    waffle::Operator::I32And => binop!(bl,vn,ra,rb,args,(#ra & #rb)),
                    waffle::Operator::I32Or => binop!(bl,vn,ra,rb,args,(#ra | #rb)),
                    waffle::Operator::I32Xor => binop!(bl,vn,ra,rb,args,(#ra ^ #rb)),
                    waffle::Operator::I32Shl => binop!(bl,vn,ra,rb,args,(#ra << #rb)),
                    waffle::Operator::I32ShrS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u32>((std::mem::transmute::<_,i32>(#ra) >> std::mem::transmute::<_,i32>(#rb)))
                    })),
                    waffle::Operator::I32ShrU => binop!(bl,vn,ra,rb,args,(#ra >> #rb)),
                    waffle::Operator::I32Rotl => binop!(bl,vn,ra,rb,args,(#ra.rotate_left(#rb))),
                    waffle::Operator::I32Rotr => binop!(bl,vn,ra,rb,args,(#ra.rotate_right(#rb))),
                    waffle::Operator::I64Clz => unop!(bl,vn,ra,args,(#ra.leading_zeros() as u64)),
                    waffle::Operator::I64Ctz => unop!(bl,vn,ra,args,(#ra.trailing_zeros() as u64)),
                    waffle::Operator::I64Popcnt => unop!(bl,vn,ra,args,(#ra.count_ones() as u64)),
                    waffle::Operator::I64Add => binop!(bl,vn,ra,rb,args,(#ra.wrapping_add(#rb))),
                    waffle::Operator::I64Sub => binop!(bl,vn,ra,rb,args,(#ra.wrapping_sub(#rb))),
                    waffle::Operator::I64Mul => binop!(bl,vn,ra,rb,args,(#ra.wrapping_mul(#rb))),
                    waffle::Operator::I64DivS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) / std::mem::transmute::<_,i64>(#rb)))
                    })),
                    waffle::Operator::I64DivU => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
                    waffle::Operator::I64RemS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) % std::mem::transmute::<_,i64>(#rb)))
                    })),
                    waffle::Operator::I64RemU => binop!(bl,vn,ra,rb,args,(#ra % #rb)),
                    waffle::Operator::I64And => binop!(bl,vn,ra,rb,args,(#ra & #rb)),
                    waffle::Operator::I64Or => binop!(bl,vn,ra,rb,args,(#ra | #rb)),
                    waffle::Operator::I64Xor => binop!(bl,vn,ra,rb,args,(#ra ^ #rb)),
                    waffle::Operator::I64Shl => binop!(bl,vn,ra,rb,args,(#ra << #rb)),
                    waffle::Operator::I64ShrS => binop!(bl,vn,ra,rb,args,(unsafe{
                        std::mem::transmute::<_,u64>((std::mem::transmute::<_,i64>(#ra) >> std::mem::transmute::<_,i64>(#rb)))
                    })),
                    waffle::Operator::I64ShrU => binop!(bl,vn,ra,rb,args,(#ra >> #rb)),
                    waffle::Operator::I64Rotl => {
                        binop!(bl,vn,ra,rb,args,(#ra.rotate_left(#rb.try_into().unwrap())))
                    }
                    waffle::Operator::I64Rotr => {
                        binop!(bl,vn,ra,rb,args,(#ra.rotate_right(#rb.try_into().unwrap())))
                    }
                    waffle::Operator::F32Abs => unop!(bl,vn,ra,args,((#ra).abs())),
                    waffle::Operator::F32Neg => unop!(bl,vn,ra,args,(0f32-#ra)),
                    waffle::Operator::F32Ceil => unop!(bl,vn,ra,args,(#ra.ceil())),
                    waffle::Operator::F32Floor => unop!(bl,vn,ra,args,(#ra.floor())),
                    waffle::Operator::F32Trunc => unop!(bl,vn,ra,args,(#ra.trunc())),
                    waffle::Operator::F32Nearest => unop!(bl,vn,ra,args,(#ra.round())),
                    waffle::Operator::F32Sqrt => unop!(bl,vn,ra,args,(#ra.sqrt())),
                    waffle::Operator::F32Add => binop!(bl,vn,ra,rb,args,(#ra + #rb)),
                    waffle::Operator::F32Sub => binop!(bl,vn,ra,rb,args,(#ra - #rb)),
                    waffle::Operator::F32Mul => binop!(bl,vn,ra,rb,args,(#ra * #rb)),
                    waffle::Operator::F32Div => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
                    waffle::Operator::F32Min => binop!(bl,vn,ra,rb,args,(#ra.min(#rb))),
                    waffle::Operator::F32Max => binop!(bl,vn,ra,rb,args,(#ra.max(#rb))),
                    waffle::Operator::F32Copysign => {
                        binop!(bl,vn,ra,rb,args,(#rb.signum() * #ra.abs()))
                    }

                    waffle::Operator::F64Abs => unop!(bl,vn,ra,args,((#ra).abs())),
                    waffle::Operator::F64Neg => unop!(bl,vn,ra,args,(0f64-#ra)),
                    waffle::Operator::F64Ceil => unop!(bl,vn,ra,args,(#ra.ceil())),
                    waffle::Operator::F64Floor => unop!(bl,vn,ra,args,(#ra.floor())),
                    waffle::Operator::F64Trunc => unop!(bl,vn,ra,args,(#ra.trunc())),
                    waffle::Operator::F64Nearest => unop!(bl,vn,ra,args,(#ra.round())),
                    waffle::Operator::F64Sqrt => unop!(bl,vn,ra,args,(#ra.sqrt())),
                    waffle::Operator::F64Add => binop!(bl,vn,ra,rb,args,(#ra + #rb)),
                    waffle::Operator::F64Sub => binop!(bl,vn,ra,rb,args,(#ra - #rb)),
                    waffle::Operator::F64Mul => binop!(bl,vn,ra,rb,args,(#ra * #rb)),
                    waffle::Operator::F64Div => binop!(bl,vn,ra,rb,args,(#ra / #rb)),
                    waffle::Operator::F64Min => binop!(bl,vn,ra,rb,args,(#ra.min(#rb))),
                    waffle::Operator::F64Max => binop!(bl,vn,ra,rb,args,(#ra.max(#rb))),
                    waffle::Operator::F64Copysign => {
                        binop!(bl,vn,ra,rb,args,(#rb.signum() * #ra.abs()))
                    }

                    waffle::Operator::I32WrapI64 => unop!(bl,vn,ra,args,((#ra & 0xfffffff) as u32)),
                    waffle::Operator::I32TruncF32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc() as i32)}))
                    }
                    waffle::Operator::I32TruncF32U => unop!(bl,vn,ra,args,(#ra.trunc() as u32)),
                    waffle::Operator::I32TruncF64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc() as i32)}))
                    }
                    waffle::Operator::I32TruncF64U => unop!(bl,vn,ra,args,(#ra.trunc() as u32)),
                    waffle::Operator::I64TruncF32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u64>(#ra.trunc() as i64)}))
                    }
                    waffle::Operator::I64TruncF32U => unop!(bl,vn,ra,args,(#ra.trunc() as u64)),
                    waffle::Operator::I64TruncF64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u64>(#ra.trunc() as i64)}))
                    }
                    waffle::Operator::I64TruncF64U => unop!(bl,vn,ra,args,(#ra.trunc() as u64)),
                    waffle::Operator::I64ExtendI32S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u64>(std::mem::transmute::<u32,i32>(#ra) as i64)
                    })),
                    waffle::Operator::I64ExtendI32U => unop!(bl,vn,ra,args,(#ra as u64)),
                    waffle::Operator::F32ConvertI32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i32>(#ra) as f32}))
                    }
                    waffle::Operator::F32ConvertI32U => unop!(bl,vn,ra,args,(unsafe{#ra as f32})),
                    waffle::Operator::F32ConvertI64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i64>(#ra) as f32}))
                    }
                    waffle::Operator::F32ConvertI64U => unop!(bl,vn,ra,args,(unsafe{#ra as f32})),
                    waffle::Operator::F32DemoteF64 => unop!(bl,vn,ra,args,(#ra as f32)),

                    waffle::Operator::F64ConvertI32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i32>(#ra) as f64}))
                    }
                    waffle::Operator::F64ConvertI32U => unop!(bl,vn,ra,args,(unsafe{#ra as f64})),
                    waffle::Operator::F64ConvertI64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,i64>(#ra) as f64}))
                    }
                    waffle::Operator::F64ConvertI64U => unop!(bl,vn,ra,args,(unsafe{#ra as f64})),

                    waffle::Operator::F64PromoteF32 => unop!(bl,vn,ra,args,(#ra as f64)),
                    waffle::Operator::I32Extend8S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u32>(std::mem::transmute::<u8,i8>((#ra & 0xff) as u8) as i32)
                    })),
                    waffle::Operator::I32Extend16S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u32>(std::mem::transmute::<u16,i16>((#ra & 0xffff) as u16) as i32)
                    })),
                    waffle::Operator::I64Extend8S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u64>(std::mem::transmute::<u8,i8>((#ra & 0xff) as u8) as i64)
                    })),
                    waffle::Operator::I64Extend16S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u64>(std::mem::transmute::<u16,i16>((#ra & 0xffff) as u16) as i64)
                    })),
                    waffle::Operator::I64Extend32S => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u64>(std::mem::transmute::<u32,i32>((#ra & 0xffffffff) as u32) as i64)
                    })),
                    // waffle::Operator::I32TruncSatF32S => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I32TruncSatF32U => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I32TruncSatF64S => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I32TruncSatF64U => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I64TruncSatF32S => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I64TruncSatF32U => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I64TruncSatF64S => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    // waffle::Operator::I64TruncSatF64U => {
                    //     panic!("not yet implemented: {}; {}", file!(), line!())
                    // }
                    waffle::Operator::I32TruncSatF32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i32>())}))
                    }
                    waffle::Operator::I32TruncSatF32U => {
                        unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u32>()))
                    }
                    waffle::Operator::I32TruncSatF64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i32>())}))
                    }
                    waffle::Operator::I32TruncSatF64U => {
                        unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u32>()))
                    }
                    waffle::Operator::I64TruncSatF32S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i64>())}))
                    }
                    waffle::Operator::I64TruncSatF32U => {
                        unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u64>()))
                    }
                    waffle::Operator::I64TruncSatF64S => {
                        unop!(bl,vn,ra,args,(unsafe{std::mem::transmute::<_,u32>(#ra.trunc().saturating_as::<i64>())}))
                    }
                    waffle::Operator::I64TruncSatF64U => {
                        unop!(bl,vn,ra,args,(#ra.trunc().saturating_as::<u64>))
                    }
                    waffle::Operator::F32ReinterpretI32 => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,f32>(#ra)
                    })),
                    waffle::Operator::F64ReinterpretI64 => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,f64>(#ra)
                    })),
                    waffle::Operator::I32ReinterpretF32 => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u32>(#ra)
                    })),
                    waffle::Operator::I64ReinterpretF64 => unop!(bl,vn,ra,args,(unsafe{
                        std::mem::transmute::<_,u64>(#ra)
                    })),
                    waffle::Operator::TableGet { table_index } => {
                        panic!("not yet implemented: {}; {}", file!(), line!())
                    }
                    waffle::Operator::TableSet { table_index } => {
                        panic!("not yet implemented: {}; {}", file!(), line!())
                    }
                    waffle::Operator::TableGrow { table_index } => {
                        panic!("not yet implemented: {}; {}", file!(), line!())
                    }
                    waffle::Operator::TableSize { table_index } => {
                        panic!("not yet implemented: {}; {}", file!(), line!())
                    }
                    waffle::Operator::MemorySize { mem } => {
                        let m = Ident::new(&mem.to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            let #vn = (this.#m().len() / 65536) as u32;
                        }
                    }
                    waffle::Operator::MemoryGrow { mem } => {
                        let m = Ident::new(&mem.to_string(), Span::call_site());
                        let a = Ident::new(&args[0].to_string(), Span::call_site());
                        bl = quote! {
                            #bl;
                            let #vn = this.#m().len() / 65536;
                            let l = this.#m().len();
                            this.#m().resize(l + (#a as usize) * 65536,0);
                            let #vn = #vn as u32
                        }
                    }
                    waffle::Operator::MemoryCopy { dst_mem, src_mem } => {
                        let dst_mem = Ident::new(&dst_mem.to_string(), Span::call_site());
                        let src_mem = Ident::new(&src_mem.to_string(), Span::call_site());
                        ternop!(bl,vn,ra,rb,rc,args,({
                            let v = this.#src_mem()[(#ra as usize)..][..(#rc as usize)].to_vec();
                            this.#dst_mem()[(#rb as usize)..][..(#rc as usize)].copy_from_slice(&v);
                            0u32
                        }))
                    }
                    waffle::Operator::MemoryFill { mem } => {
                        let mem = Ident::new(&mem.to_string(), Span::call_site());
                        ternop!(bl,vn,ra,rb,rc,args,({
                            this.#mem()[(#ra as usize)..][..(#rc as usize)].fill((#rb & 0xff) as u8);
                            0u32
                        }))
                    },
                    o => panic!("nyi operator: {o}")
                }
            }
            waffle::ValueDef::PickOutput(v, w, _) => {
                let value = Ident::new(&v.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn  = #value.#w
                }
            }
            waffle::ValueDef::Alias(l) => {
                let l = Ident::new(&l.to_string(), Span::call_site());
                bl = quote! {
                    #bl;
                    let #vn = #l
                }
            }
            waffle::ValueDef::Placeholder(_) => {}
            waffle::ValueDef::Trace(_, _) => {
                panic!("not yet implemented: {}; {}", file!(), line!())
            }
            waffle::ValueDef::None => {}
        }
    }
    fn block_target(
        b: &FunctionBody,
        f: Func,
        m: &Module,
        t: &BlockTarget,
        ident: Ident,
        opts: &Opts,
    ) -> proc_macro2::TokenStream {
        let values = t
            .args
            .iter()
            .map(|a| format!("{a}"))
            .map(|v| Ident::new(&v, Span::call_site()))
            .collect::<Vec<_>>();
        let s = Ident::new(&format!("{ident}{f}State"), ident.span());
        let func = Ident::new(&f.to_string(), Span::call_site());
        let f = Ident::new(&t.block.to_string(), Span::call_site());
        if opts.needs_tco() {
            let mut fc = quote! {
                this.#func (state)
            };
            if opts.r#async {
                fc = quote! {
                    #fc .await
                }
            }
            quote! {
                {
                state = #s::#f(#(#values),*);
                return #fc;
                }
            }
        } else {
            quote! {
                {
                state = #s::#f(#(#values),*);
                continue;
                }
            }
        }
    }
    let t = match &b.blocks[a].terminator {
        waffle::Terminator::Br { target } => block_target(b, f, m, target, ident.clone(), opts),
        waffle::Terminator::CondBr {
            cond,
            if_true,
            if_false,
        } => {
            let cond = Ident::new(&cond.to_string(), Span::call_site());
            let if_true = block_target(b, f, m, if_true, ident.clone(), opts);
            let if_false = block_target(b, f, m, if_false, ident.clone(), opts);
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
            let value = Ident::new(&value.to_string(), Span::call_site());
            let default = block_target(b, f, m, default, ident.clone(), opts);
            let mut t2 = BTreeMap::new();
            let mut i = 0u32;
            for t in targets.iter() {
                t2.insert(i, block_target(b, f, m, t, ident.clone(), opts));
                i += 1;
            }
            let mut b = quote! {};
            for (k, v) in t2.iter() {
                b = quote! {
                    #b
                    #k => {#v}
                }
            }
            b = quote! {
                #b
                _ => #default
            };
            quote! {
                match #value{
                    #b
                }
            }
        }
        waffle::Terminator::Return { values } => {
            let values = values
                .iter()
                .map(|a| format!("{a}"))
                .map(|v| Ident::new(&v, Span::call_site()))
                .collect::<Vec<_>>();
            let mut q = quote! {
                (#(#values),*,)
            };
            if values.len() == 0 {
                q = quote! {()}
            };
            if opts.r#impl {
                q = quote! {
                    (#q,this)
                }
            }
            if opts.result {
                q = quote! {Ok(#q)}
            }
            quote! {
                return #q;
            }
        }
        _ => {
            quote! {
                #unr
            }
        }
    };
    let s = Ident::new(&format!("{ident}{f}State"), ident.span());
    return Ok(quote! {
        #s::#bn(#(#bp),*) => {
            #bl;
            #t;
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn assert_normal() {
    //     let a = include_bytes!("../../target/wasm32-unknown-unknown/release/sample_plugin.wasm");
    //     let l = lower(a);
    //     assert!(l.is_ok());
    // }
}
