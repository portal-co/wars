use relooper::ShapedBlock;
use waffle::{cfg::CFGInfo, Block, FunctionBody};

pub fn go(b: &FunctionBody) -> Box<ShapedBlock<Block>> {
    let cfg = CFGInfo::new(b);
    let reloop = std::panic::catch_unwind(|| {
        relooper::reloop(
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
                // .chain(once((Block::invalid(), vec![b.entry])))
                .collect(),
            // Block::invalid(),
            b.entry,
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
    reloop
}
