use std::{sync::{Arc, Mutex}, vec::Vec};

use dumpster::{sync::Gc, Trace};
// use ic_stable_structures::Vec;

mod heapsize{
    pub trait HeapSize{}
    impl<T: ?Sized> HeapSize for T{}
}

#[derive(Clone)]
#[non_exhaustive]
pub enum GcCore<R>{
    Fields(Vec<Field<R>>)
}

unsafe impl<R: Trace> Trace for GcCore<R>{
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self{
            GcCore::Fields(vec) => vec.accept(visitor),
        }
    }
}

#[derive(Clone)]
#[non_exhaustive]
pub enum Field<R>{
    Const(R),
    Mut(Arc<Mutex<R>>)
}
unsafe impl<R: Trace> Trace for Field<R>{
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self{
            Field::Const(a) => a.accept(visitor),
            Field::Mut(arc) => arc.as_ref().accept(visitor),
        }
    }
}