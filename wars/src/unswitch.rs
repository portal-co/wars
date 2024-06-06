use std::iter::empty;

use waffle::{
    Block, ExportKind, Func, FuncDecl, Import, ImportKind, Memory, MemoryArg, MemoryData, Module,
    Operator, Signature, SignatureData, Table, Type, Value, ValueDef,
};
use waffle::{BlockTarget, FunctionBody, Terminator};

pub fn go(f: &mut FunctionBody) {
    let vz = f.arg_pool.from_iter(empty());
    let tz = f.type_pool.from_iter(empty());
    let ti = f.type_pool.from_iter(vec![Type::I32].into_iter());
    let ia = f.add_value(ValueDef::Operator(Operator::I32Const { value: 1 }, vz, ti));
    f.append_to_block(f.entry, ia);
    loop {
        let mut i = f.blocks.entries();
        let (b, value, targets, default) = 'gather: loop {
            let Some(a) = i.next() else {
                drop(i);
                f.recompute_edges();
                return;
            };
            if let Terminator::Select {
                value,
                targets,
                default,
            } = a.1.clone().terminator
            {
                break 'gather (a.0, value, targets.clone(), default);
            }
        };
        drop(i);
        f.blocks[b].terminator = Terminator::None;
        if targets.len() == 0 {
            f.set_terminator(b, Terminator::Br { target: default });
            continue;
        }
        if targets.len() == 1 {
            f.set_terminator(
                b,
                Terminator::CondBr {
                    cond: value,
                    if_true: default,
                    if_false: targets[0].clone(),
                },
            );
            continue;
        }
        let t2 = targets[1..].to_vec();
        let n = f.add_block();
        let vs = f.arg_pool.from_iter(vec![value, ia].into_iter());
        let ic = f.add_value(ValueDef::Operator(Operator::I32Sub, vs, ti));
        f.append_to_block(n, ic);
        f.set_terminator(
            n,
            Terminator::Select {
                value: ic,
                targets: t2,
                default: default,
            },
        );
        f.set_terminator(
            b,
            Terminator::CondBr {
                cond: value,
                if_true: BlockTarget {
                    block: n,
                    args: vec![],
                },
                if_false: targets[0].clone(),
            },
        );
    }
}
