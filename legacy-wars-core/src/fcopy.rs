
use std::collections::{BTreeMap, BTreeSet, HashMap};

use waffle::{
    cfg::CFGInfo, entity::EntityRef, pool::ListRef, Block, BlockTarget, FrontendOptions, Func,
    FunctionBody, Memory, MemoryArg, Module, Operator, Signature, SignatureData, Terminator, Type,
    Value, ValueDef,
};
pub fn add_op(f: &mut FunctionBody, args: &[Value], rets: &[Type], op: Operator) -> Value {
    let args = f.arg_pool.from_iter(args.iter().map(|a| *a));
    let rets = f.type_pool.from_iter(rets.iter().map(|a| *a));
    return f.add_value(ValueDef::Operator(op, args, rets));
}

use waffle::util::new_sig;
pub trait Obfuscate {
    fn boot(&mut self, b: Block, f: &mut FunctionBody) -> anyhow::Result<Block> {
        return Ok(b);
    }
    fn sig(&mut self, a: SignatureData) -> anyhow::Result<SignatureData> {
        return Ok(a);
    }
    fn obf(
        &mut self,
        o: Operator,
        f: &mut FunctionBody,
        b: Block,
        args: &[Value],
        types: &[Type],
        module: &mut Module,
    ) -> anyhow::Result<(Value, Block)>;
    fn obf_term(&mut self, t: Terminator, b: Block, f: &mut FunctionBody) -> anyhow::Result<()> {
        f.set_terminator(b, t);
        return Ok(());
    }
}
pub struct DontObf {}
impl Obfuscate for DontObf {
    fn obf(
        &mut self,
        o: Operator,
        f: &mut FunctionBody,
        b: Block,
        args: &[Value],
        types: &[Type],
        module: &mut Module,
    ) -> anyhow::Result<(Value, Block)> {
        let v = add_op(f, args, types, o);
        f.append_to_block(b, v);
        return Ok((v, b));
    }
}
pub fn tweak_value(
    f: &mut FunctionBody,
    ba: &FunctionBody,
    x: &mut ValueDef,
    mut m: impl FnMut(&mut Value),
    b: Block,
) {
    match x {
        ValueDef::BlockParam(a, _, _) => *a = b,
        ValueDef::Operator(o, l, t) => {
            let mut ls = ba.arg_pool[*l].to_vec();
            // eprintln!("l={ls:?};o={o}");
            for v in ls.iter_mut() {
                // eprintln!("l={v}");
                m(v)
            }
            *l = f.arg_pool.from_iter(ls.into_iter());
            *t = f
                .type_pool
                .from_iter(ba.type_pool[*t].iter().map(|a| a.clone()));
        }
        ValueDef::PickOutput(v, _, _) => m(v),
        ValueDef::Alias(v) => m(v),
        ValueDef::Placeholder(_) => todo!(),
        ValueDef::Trace(_, l) => {
            let mut ls = ba.arg_pool[*l].to_vec();
            for v in ls.iter_mut() {
                m(v)
            }
            *l = f.arg_pool.from_iter(ls.into_iter());
        }
        ValueDef::None => {}
    }
}
pub fn tweak_target(
    f: &mut FunctionBody,
    x: &mut BlockTarget,
    mut m: impl FnMut(&mut Value),
    mut k: impl FnMut(&mut Block),
) {
    k(&mut x.block);
    for a in &mut x.args {
        m(a)
    }
}
pub fn tweak_terminator(
    f: &mut FunctionBody,
    x: &mut Terminator,
    mut m: impl FnMut(&mut Value),
    mut k: impl FnMut(&mut Block),
) {
    match x {
        Terminator::Br { target } => tweak_target(f, target, m, k),
        Terminator::CondBr {
            cond,
            if_true,
            if_false,
        } => {
            m(cond);
            tweak_target(f, if_true, &mut m, &mut k);
            tweak_target(f, if_false, m, k);
        }
        Terminator::Select {
            value,
            targets,
            default,
        } => {
            m(value);
            for target in targets {
                tweak_target(f, target, &mut m, &mut k)
            }
            tweak_target(f, default, m, k)
        }
        Terminator::Return { values } => {
            for a in values {
                m(a)
            }
        }
        Terminator::Unreachable => {}
        Terminator::None => {}
        Terminator::ReturnCall { func, args } => {
            for a in args {
                m(a)
            }
        }
        Terminator::ReturnCallIndirect { sig, table, args } => {
            for a in args {
                m(a)
            }
        }
        Terminator::ReturnCallRef { sig, args } => {
            for a in args {
                m(a)
            }
        }
    }
}
pub fn clone_value(
    basis: &FunctionBody,
    f: &mut FunctionBody,
    mut m: impl FnMut(&mut Value),
    v: Value,
    b: &mut Block,
    obf: &mut impl Obfuscate,
    module: &mut Module,
) -> anyhow::Result<Value> {
    let mut w = basis.values.get(v).map(|b| b.clone()).unwrap();
    if let ValueDef::Operator(a, d, c) = w {
        let d = basis.arg_pool[d]
            .iter()
            .map(|a| {
                let mut a = *a;
                m(&mut a);
                return a;
            })
            .collect::<Vec<_>>();
        let c = basis.type_pool[c].to_vec();
        // if c.len() == 1 {
        //HACK: fix
        let (v, d) = obf.obf(a, f, *b, &d, &c, module)?;
        *b = d;
        return Ok(v);
        // }
    }
    tweak_value(f, basis, &mut w, m, *b);
    let w = f.add_value(w);
    f.append_to_block(*b, w);
    Ok(w)
}
pub fn clone_value_in(
    basis: &FunctionBody,
    f: &mut FunctionBody,
    m: &mut HashMap<Value, Value>,
    v: &mut Value,
    b: &mut Block,
    o: Block,
    depth: usize,
    obf: &mut impl Obfuscate,
    module: &mut Module,
) -> anyhow::Result<Value> {
    if depth == 0 {
        let mut p = String::new();
        if let ValueDef::Operator(a, b, c) = basis.values[*v] {
            p = format!("{p};params={:?}", &basis.arg_pool[b])
        }
        panic!("exceeded depth: {} ({:?} {p})", *v, basis.values[*v]);
    }
    // if let ValueDef::BlockParam(_, p, _) = basis.values[*v]{
    //     *v = f.blocks[b].params[p as usize].1;
    //     return *v;
    // }
    // loop {
    if let Some(n) = m.get(&*v) {
        // *v = *n;
        return Ok(*v);
    }
    // let n = f.add_value(ValueDef::None);
    let g = f as *mut FunctionBody;
    // let mut r = vec![];
    let n = clone_value(
        basis,
        f,
        |a| {
            *a = match m.get(&a) {
                None => {
                    // if basis.value_blocks[*a] == Block::invalid() {
                    //     *a = n;
                    //     return;
                    // }
                    // clone_value_in(basis, unsafe { &mut *g }, m, a, b, o, depth - 1);
                    // eprintln!("{}", basis.display("", None));
                    // eprintln!("{}; {}", o, b);
                    let mut p = format!(
                        "block={};m={:?};in={}",
                        basis.value_blocks[*a],
                        m.clone(),
                        *v
                    );
                    if let ValueDef::Operator(a, b, c) = basis.values[*a] {
                        p = format!("{p};params={:?}", &basis.arg_pool[b])
                    }
                    panic!("value not found: {a} ({:?} {p})", basis.values[*a]);
                    // r.push(*a);
                    // *a
                    *a
                }
                Some(b) => b.clone(),
            }
        },
        v.clone(),
        b,
        obf,
        module,
    )?;
    // eprintln!("{v} => {n}");
    m.insert((*v).clone(), n.clone());
    *v = n;
    // f.append_to_block(*b, n);
    return Ok(n);
    // }
}
pub fn clone_block(
    f: &mut FunctionBody,
    basis: &FunctionBody,
    b: Block,
    new: Block,
    mut k: impl FnMut(&mut Block),
    obf: &mut impl Obfuscate,
    modu: &mut Module,
) -> anyhow::Result<()> {
    let mut d = basis.blocks.get(b).unwrap().clone();
    let mut m: HashMap<Value, Value> = HashMap::new();
    let mut c = BTreeSet::new();
    let mut r = new;
    for (pt, pv) in d.params.iter_mut() {
        let npv = f.add_blockparam(r, *pt);
        m.insert(*pv, npv);
        *pv = npv;
    }
    r = obf.boot(r, f)?;
    // eprintln!("insts={:?}",d.insts);
    for v in &mut d.insts {
        if let Some(_) = m.get(v) {
            continue;
        }
        if c.contains(v) {
            continue;
        }
        c.insert(*v);
        // eprintln!("b={b},v={v}");
        if basis.value_blocks[*v] != b {
            panic!("inconsistent block value: {}", *v);
        }
        clone_value_in(basis, f, &mut m, v, &mut r, b, 100, obf, modu)?;
    }
    tweak_terminator(
        f,
        &mut d.terminator,
        |a| {
            *a = match m.get(&a) {
                None => a.clone(),
                Some(b) => b.clone(),
            }
        },
        k,
    );
    // let mut c = f.blocks.get_mut(r).unwrap();
    // for a in d.insts.clone(){
    //     f.append_to_block(r, a);
    // }
    obf.obf_term(d.terminator.clone(), r, f)?;
    return Ok(());
}
pub struct FunCloneRes {
    pub all: BTreeMap<Block, Block>,
}
pub fn sanity(body: &FunctionBody) {
    let mut uses = BTreeSet::default();
    for block in body.blocks.iter() {
        for &inst in &body.blocks[block].insts {
            match &body.values[inst] {
                &ValueDef::Operator(_, args, _) | &ValueDef::Trace(_, args) => {
                    for &arg in &body.arg_pool[args] {
                        let arg = body.resolve_alias(arg);
                        uses.insert(arg);
                    }
                }
                &ValueDef::PickOutput(value, ..) => {
                    let value = body.resolve_alias(value);
                    uses.insert(value);
                }
                _ => {}
            }
        }
        body.blocks[block].terminator.visit_uses(|u| {
            let u = body.resolve_alias(u);
            uses.insert(u);
        });
    }
    for (v, w) in body.values.entries() {
        if w.ty(&body.type_pool).is_none() && uses.contains(&v) {
            panic!("invalid type: {v} in {}", body.display("", None));
        }
    }
}
pub fn clone_fn(
    f: &mut FunctionBody,
    basis: &FunctionBody,
    obf: &mut impl Obfuscate,
    m: &mut Module,
) -> anyhow::Result<FunCloneRes> {
    let mut basis = basis.clone();
    sanity(&basis);
    basis.convert_to_max_ssa(None);
    let mut all = BTreeMap::new();
    for k in basis.blocks.entries().map(|a| a.0) {
        all.insert(k, f.add_block());
    }
    for (a, b) in all.clone() {
        clone_block(f, &basis, a, b, |k| *k = *all.get(k).unwrap(), obf, m)?;
    }
    return Ok(FunCloneRes { all });
}
pub fn obf_fn(m: &mut Module, f: Func, obf: &mut impl Obfuscate) -> anyhow::Result<()> {
    let s = m.funcs[f].sig();
    if let Some(b) = m.funcs[f].body() {
        let b = b.clone();
        let s = new_sig(m, obf.sig(m.signatures[s].clone())?);
        let mut n = FunctionBody::new(&m, s);
        let r = clone_fn(&mut n, &b, obf, m)?;
        n.entry = *r.all.get(&b.entry).unwrap();
        *m.funcs[f].body_mut().unwrap() = n;
    }
    return Ok(());
}
pub fn obf_mod(m: &mut Module, obf: &mut impl Obfuscate) -> anyhow::Result<()> {
    for f in m.funcs.iter().map(|a| a).collect::<Vec<_>>() {
        obf_fn(m, f, obf)?;
    }
    return Ok(());
}
