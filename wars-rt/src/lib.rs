#![no_std]
extern crate alloc;

pub mod func;
pub mod wasix;
use alloc::{boxed::Box, sync::Arc, vec::Vec};
use core::iter::empty;

#[cfg(feature = "std")]
pub use std::sync::Mutex;

#[cfg(not(feature = "std"))]
pub use spin::Mutex;

pub trait Err: Into<anyhow::Error> {}
impl<T: Into<anyhow::Error>> Err for T {}

#[cfg(feature = "std")]
extern crate std;

#[derive(Clone)]
pub enum Pit<X, H> {
    Guest { id: [u8; 32], x: X, s: [u8; 32] },
    Host { host: H },
}
// use as_ref::AsSlice;
// use func::CtxSpec;
pub use func::Value;
pub trait CtxSpec: Sized {
    type ExternRef: Clone;
}
pub trait Traverse<C: CtxSpec> {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a C::ExternRef> + 'a>;
    fn traverse_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut C::ExternRef> + 'a>;
}
impl<C: CtxSpec, V: Traverse<C>> Traverse<C> for Vec<V> {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(self.iter().flat_map(|a| a.traverse()))
    }

    fn traverse_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(self.iter_mut().flat_map(|x| x.traverse_mut()))
    }
}
impl<C: CtxSpec> Traverse<C> for u32 {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(empty())
    }

    fn traverse_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(empty())
    }
}
impl<C: CtxSpec> Traverse<C> for u64 {
    fn traverse<'a>(&'a self) -> Box<dyn Iterator<Item = &'a <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(empty())
    }

    fn traverse_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut <C as CtxSpec>::ExternRef> + 'a> {
        Box::new(empty())
    }
}
pub trait Memory {
    fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>>;
    fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()>;
    fn size(&self) -> anyhow::Result<u64>;
    fn grow(&mut self, x: u64) -> anyhow::Result<()>;
}
#[cfg(feature = "ic-stable-structures")]
pub mod ic {
    use alloc::{boxed::Box, vec};

    #[repr(transparent)]
    pub struct Stable<T>(pub T);

    impl<T: ic_stable_structures::Memory> super::Memory for Stable<T> {
        fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
            let mut v = vec![0u8; s as usize];
            self.0.read(a, &mut v);
            Ok(Box::new(v))
        }

        fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
            self.0.write(a, x);
            Ok(())
        }

        fn size(&self) -> anyhow::Result<u64> {
            let s = self.0.size();
            Ok(s * 65536)
        }

        fn grow(&mut self, x: u64) -> anyhow::Result<()> {
            if self.0.grow((x + 65535) / 65536) == -1 {
                anyhow::bail!("stable growth failed")
            }
            Ok(())
        }
    }
}
impl Memory for Vec<u8> {
    fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
        Ok(Box::new(&self[(a as usize)..][..(s as usize)]))
    }

    fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
        self[(a as usize)..][..x.len()].copy_from_slice(x);
        Ok(())
    }

    fn size(&self) -> anyhow::Result<u64> {
        Ok(self.len() as u64)
    }

    fn grow(&mut self, x: u64) -> anyhow::Result<()> {
        self.extend((0..x).map(|a| 0u8));
        Ok(())
    }
}
impl<T: Memory + ?Sized> Memory for Box<T> {
    fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
        self.as_ref().read(a, s)
    }

    fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
        self.as_mut().write(a, x)
    }

    fn size(&self) -> Result<u64, anyhow::Error> {
        self.as_ref().size()
    }

    fn grow(&mut self, x: u64) -> anyhow::Result<()> {
        self.as_mut().grow(x)
    }
}
#[cfg(feature = "std")]
impl<T: Memory> Memory for Arc<std::sync::Mutex<T>> {
    fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
        let l = self.lock().unwrap();
        let r = l.read(a, s)?;
        return Ok(Box::new(r.as_ref().as_ref().to_vec()));
    }

    fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
        let mut l = self.lock().unwrap();
        return l.write(a, x);
    }

    fn size(&self) -> Result<u64, anyhow::Error> {
        let l = self.lock().unwrap();
        return l.size();
    }

    fn grow(&mut self, x: u64) -> anyhow::Result<()> {
        let mut l = self.lock().unwrap();
        return l.grow(x);
    }
}
#[cfg(not(feature = "std"))]
impl<T: Memory> Memory for Arc<spin::Mutex<T>> {
    fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
        let l = self.lock();
        let r = l.read(a, s)?;
        return Ok(Box::new(r.as_ref().as_ref().to_vec()));
    }

    fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
        let mut l = self.lock();
        return l.write(a, x);
    }

    fn size(&self) -> Result<u64, anyhow::Error> {
        let l = self.lock();
        return l.size();
    }

    fn grow(&mut self, x: u64) -> anyhow::Result<()> {
        let mut l = self.lock();
        return l.grow(x);
    }
}
pub unsafe fn host_memory() -> impl Memory {
    struct W {}
    impl Memory for W {
        fn read<'a>(&'a self, a: u64, s: u64) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
            return Ok(Box::new(unsafe {
                core::slice::from_raw_parts(a as usize as *const u8, s as usize)
            }));
        }

        fn write(&mut self, a: u64, x: &[u8]) -> anyhow::Result<()> {
            let n = unsafe { core::slice::from_raw_parts_mut(a as usize as *mut u8, x.len()) };
            n.copy_from_slice(x);
            return Ok(());
        }

        fn size(&self) -> Result<u64, anyhow::Error> {
            anyhow::bail!("host memory cannot use size")
        }

        fn grow(&mut self, x: u64) -> anyhow::Result<()> {
            anyhow::bail!("host memory cannot use grow")
        }
    }
    return W {};
}
pub mod _rexport {
    pub use anyhow;
    pub use tramp;
    pub use tuple_list;
}
macro_rules! int_ty{
    ($int:ty => $p:ident) => {
        paste::paste!{
            pub fn [<$p add>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.wrapping_add(b)))
            }
            pub fn [<$p mul>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.wrapping_mul(b)))
            }
            pub fn [<$p and>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a & b))
            }
            pub fn [<$p or>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a | b))
            }
            pub fn [<$p xor>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a ^ b))
            }
            pub fn [<$p shl>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a << b))
            }
            pub fn [<$p shru>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a >> b))
            }
            pub fn [<$p shrs>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(((a as $p) >> b) as $int))
            }
            pub fn [<$p divu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a / b))
            }
            pub fn [<$p divs>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(((a as $p) / (b as $p)) as $int))
            }
            pub fn [<$p remu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a % b))
            }
            pub fn [<$p rems>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(((a as $p) % (b as $p)) as $int))
            }
            pub fn [<$p rotl>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.rotate_left((b & 0xffffffff) as u32)))
            }
            pub fn [<$p clz>](a: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.leading_zeros() as $int))
            }
            pub fn [<$p ctz>](a: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.trailing_zeros() as $int))
            }
            //comparisons
            pub fn [<$p eqz>](a: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a == 0{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p eq>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a == b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p ne>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a != b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p ltu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a < b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p gtu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a > b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p leu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a <= b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p geu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                Ok(tuple_list::tuple_list!(if a >= b{
                    1
                }else{
                    0
                }))
            }
            //signed
            pub fn [<$p lts>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                let a = a as $p;
                let b = b as $p;
                Ok(tuple_list::tuple_list!(if a < b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p gts>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                let a = a as $p;
                let b = b as $p;
                Ok(tuple_list::tuple_list!(if a > b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p les>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                let a = a as $p;
                let b = b as $p;
                Ok(tuple_list::tuple_list!(if a <= b{
                    1
                }else{
                    0
                }))
            }
            pub fn [<$p ges>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
                let a = a as $p;
                let b = b as $p;
                Ok(tuple_list::tuple_list!(if a >= b{
                    1
                }else{
                    0
                }))
            }

            pub fn [<$p sub>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a.wrapping_sub(b)))
            }
            //LOADS and STORES
            pub fn [<$p load>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,core::mem::size_of::<$int>().try_into().unwrap())?;
                Ok(tuple_list::tuple_list!($int::from_ne_bytes(r.as_ref().as_ref().try_into().unwrap())))
            }
            pub fn [<$p store>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: Err + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into().map_err(Into::into)?..][..std::mem::size_of::<$int>()];
                // r.copy_from_slice(&c.to_ne_bytes());
                a.write(b.try_into().map_err(Into::into)?,&c.to_ne_bytes())?;
                Ok(())
            }
            //8 BIT
            pub fn [<$p load8u>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,1)?.as_ref().as_ref()[0];
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load8s>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,1)?.as_ref().as_ref()[0];
                Ok(tuple_list::tuple_list!(r as i8 as $p as $int))
            }
            pub fn [<$p store8>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: Err + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into().map_err(Into::into)?..][..1];
                // r[0] = (c & 0xff) as u8;
                a.write(b.try_into().map_err(Into::into)?,&[(c & 0xff) as u8])?;
                Ok(())
            }
            //16 BIT
            pub fn [<$p load16u>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,2)?;
                let r = u16::from_ne_bytes(r.as_ref().as_ref().try_into().unwrap());
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load16s>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,2)?;
                let r = u16::from_ne_bytes(r.as_ref().as_ref().try_into().unwrap());
                Ok(tuple_list::tuple_list!(r as i16 as $p as $int))
            }
            pub fn [<$p store16>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: Err + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into().map_err(Into::into)?..][..2];
                a.write(b.try_into().map_err(Into::into)?,&((c & 0xffff) as u16).to_ne_bytes())?;
                Ok(())
            }
            //32 BIT
            pub fn [<$p load32u>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,4)?;
                let r = u32::from_ne_bytes(r.as_ref().as_ref().try_into().unwrap());
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load32s>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: Err + Send + Sync + 'static{
                let r = a.read(b.try_into().map_err(Into::into)?,4)?;
                let r = u32::from_ne_bytes(r.as_ref().as_ref().try_into().unwrap());
                Ok(tuple_list::tuple_list!(r as i32 as $p as $int))
            }
            pub fn [<$p store32>]<T: TryInto<u64>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: Err + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into().map_err(Into::into)?..][..4];
                a.write(b.try_into().map_err(Into::into)?,&((c & 0xffffffff) as u32).to_ne_bytes())?;
                Ok(())
            }
        }
    }
}
int_ty!(u32 => i32);
int_ty!(u64 => i64);
pub fn select<T>(u: u32, t: T, t2: T) -> anyhow::Result<tuple_list::tuple_list_type!(T)> {
    Ok(tuple_list::tuple_list!(if u != 0 { t } else { t2 }))
}
pub fn i32wrapi64(a: u64) -> anyhow::Result<tuple_list::tuple_list_type!(u32)> {
    return Ok(tuple_list::tuple_list!((a & 0xffffffff) as u32));
}
pub fn i64extendi32u(a: u32) -> anyhow::Result<tuple_list::tuple_list_type!(u64)> {
    Ok(tuple_list::tuple_list!(a as u64))
}
pub fn i64extendi32s(a: u32) -> anyhow::Result<tuple_list::tuple_list_type!(u64)> {
    Ok(tuple_list::tuple_list!(a as i32 as i64 as u64))
}
pub fn i64truncf64s(a: f64) -> anyhow::Result<tuple_list::tuple_list_type!(u64)> {
    Ok(tuple_list::tuple_list!(
        unsafe { a.trunc().to_int_unchecked::<i64>() } as u64
    ))
}
