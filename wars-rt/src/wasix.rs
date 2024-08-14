use crate::{CtxSpec, Memory};

pub trait XSpec: CtxSpec {
    fn wasix_memory<'a>(&'a mut self) -> &'a mut (dyn Memory + 'a);
}
