use core::iter::{empty, once};
use std::{
    boxed::Box,
    sync::{Arc, Mutex},
    vec::Vec,
};

use dumpster::{sync::Gc, Trace};

use crate::{CtxSpec, Traverse};
// use ic_stable_structures::Vec;

mod heapsize {
    pub trait HeapSize {}
    impl<T: ?Sized> HeapSize for T {}
}

#[derive(Clone)]
#[non_exhaustive]
pub enum GcCore<R> {
    Fields(Vec<Field<R>>),
}

impl<R: Clone> GcCore<R> {
    pub fn get_field(&self, a: usize) -> R {
        match self {
            GcCore::Fields(vec) => match &vec[a] {
                Field::Const(r) => r.clone(),
                Field::Mut(arc) => arc.lock().unwrap().clone(),
            },
        }
    }
    pub fn set_field(&self, a: usize, r: R) {
        match self {
            GcCore::Fields(vec) => match &vec[a] {
                Field::Const(_) => {}
                Field::Mut(arc) => {
                    *arc.lock().unwrap() = r;
                }
            },
        }
    }
}

unsafe impl<R: Trace> Trace for GcCore<R> {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            GcCore::Fields(vec) => vec.accept(visitor),
        }
    }
}

impl<C: CtxSpec, R: Traverse<C>> Traverse<C> for GcCore<R> {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a C::ExternRef> + 'a> {
        return match self {
            GcCore::Fields(vec) => Box::new(vec.iter().flat_map(|a| a.traverse())),
        };
    }

    fn traverse_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut C::ExternRef> + 'a> {
        return match self {
            GcCore::Fields(vec) => Box::new(vec.iter_mut().flat_map(|a| a.traverse_mut())),
        };
    }
}

#[derive(Clone)]
#[non_exhaustive]
pub enum Field<R> {
    Const(R),
    Mut(Arc<Mutex<R>>),
}
unsafe impl<R: Trace> Trace for Field<R> {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Field::Const(a) => a.accept(visitor),
            Field::Mut(arc) => arc.as_ref().accept(visitor),
        }
    }
}
impl<C: CtxSpec, R: Traverse<C>> Traverse<C> for Field<R> {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a <C as CtxSpec>::ExternRef> + 'a> {
        match self {
            Field::Const(a) => a.traverse(),
            Field::Mut(arc) => Box::new(empty()), //TODO: fix
        }
    }

    fn traverse_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <C as CtxSpec>::ExternRef> + 'a> {
        match self {
            Field::Const(a) => a.traverse_mut(),
            Field::Mut(arc) => Box::new(empty()), //TODO: fix
        }
    }
}
macro_rules! newty {
    ($name:ident) => {
        #[derive(Clone)]
        #[repr(transparent)]
        pub struct $name<W>(pub W);

        unsafe impl<W: Trace> Trace for $name<W> {
            fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
                self.0.accept(visitor)
            }
        }
    };
}
newty!(Struct);
newty!(Array);
newty!(Const);
newty!(Mut);