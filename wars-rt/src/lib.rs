pub mod func;
pub mod wrl;
pub mod wasix;
use std::sync::{Arc, Mutex};

// use as_ref::AsSlice;
use func::CtxSpec;
pub use func::Value;
pub trait Memory {
    fn read<'a>(&'a self, a: usize, s: usize) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>>;
    fn write(&mut self, a: usize, x: &[u8]) -> anyhow::Result<()>;
    fn size(&self) -> anyhow::Result<usize>;
    fn grow(&mut self, x: usize) -> anyhow::Result<()>;
}
impl Memory for Vec<u8> {
    fn read<'a>(&'a self, a: usize, s: usize) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>>{
        Ok(Box::new(&self[a..][..s]))
    }

    fn write(&mut self, a: usize, x: &[u8]) -> anyhow::Result<()> {
        self[a..][..x.len()].copy_from_slice(x);
        Ok(())
    }

    fn size(&self) -> anyhow::Result<usize> {
        Ok(self.len())
    }

    fn grow(&mut self, x: usize) -> anyhow::Result<()> {
        self.extend((0..x).map(|a| 0u8));
        Ok(())
    }
}
impl<T: Memory> Memory for Arc<Mutex<T>>{
    fn read<'a>(&'a self, a: usize, s: usize) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
        let l = self.lock().unwrap();
        let r = l.read(a, s)?;
        return Ok(Box::new(r.as_ref().as_ref().to_vec()));
    }

    fn write(&mut self, a: usize, x: &[u8]) -> anyhow::Result<()> {
        let mut l = self.lock().unwrap();
        return l.write(a, x);
    }

    fn size(&self) -> anyhow::Result<usize> {
        let l = self.lock().unwrap();
        return l.size();
    }

    fn grow(&mut self, x: usize) -> anyhow::Result<()> {
        let mut l = self.lock().unwrap();
        return l.grow(x)
    }
}
pub unsafe fn host_memory() -> impl Memory{
    struct W{}
    impl Memory for W{
        fn read<'a>(&'a self, a: usize, s: usize) -> anyhow::Result<Box<dyn AsRef<[u8]> + 'a>> {
            return Ok(Box::new(unsafe{
                std::slice::from_raw_parts(a as *const u8, s)
            }))
        }
    
        fn write(&mut self, a: usize, x: &[u8]) -> anyhow::Result<()> {
            let n = unsafe{
                std::slice::from_raw_parts_mut(a as *mut u8, x.len())
            };
            n.copy_from_slice(x);
            return Ok(());
        }
    
        fn size(&self) -> anyhow::Result<usize> {
            anyhow::bail!("host memory cannot use size")
        }
    
        fn grow(&mut self, x: usize) -> anyhow::Result<()> {
            anyhow::bail!("host memory cannot use grow")
        }
    }
    return W{};
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
            pub fn [<$p divu>](a: $int, b: $int) -> anyhow::Result<tuple_list::tuple_list_type!($int)> {
                Ok(tuple_list::tuple_list!(a / b))
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
            pub fn [<$p load>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,std::mem::size_of::<$int>())?;
                Ok(tuple_list::tuple_list!($int::from_ne_bytes(r.as_ref().as_ref().try_into()?)))
            }
            pub fn [<$p store>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: std::error::Error + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into()?..][..std::mem::size_of::<$int>()];
                // r.copy_from_slice(&c.to_ne_bytes());
                a.write(b.try_into()?,&c.to_ne_bytes())?;
                Ok(())
            }
            //8 BIT
            pub fn [<$p load8u>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,1)?.as_ref().as_ref()[0];
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load8s>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,1)?.as_ref().as_ref()[0];
                Ok(tuple_list::tuple_list!(r as i8 as $p as $int))
            }
            pub fn [<$p store8>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: std::error::Error + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into()?..][..1];
                // r[0] = (c & 0xff) as u8;
                a.write(b.try_into()?,&[(c & 0xff) as u8])?;
                Ok(())
            }
            //16 BIT
            pub fn [<$p load16u>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,2)?;
                let r = u16::from_ne_bytes(r.as_ref().as_ref().try_into()?);
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load16s>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,2)?;
                let r = u16::from_ne_bytes(r.as_ref().as_ref().try_into()?);
                Ok(tuple_list::tuple_list!(r as i16 as $p as $int))
            }
            pub fn [<$p store16>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: std::error::Error + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into()?..][..2];
                a.write(b.try_into()?,&((c & 0xffff) as u16).to_ne_bytes())?;
                Ok(())
            }
            //32 BIT
            pub fn [<$p load32u>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,4)?;
                let r = u32::from_ne_bytes(r.as_ref().as_ref().try_into()?);
                Ok(tuple_list::tuple_list!(r as $int))
            }
            pub fn [<$p load32s>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T) -> anyhow::Result<tuple_list::tuple_list_type!($int)> where T::Error: std::error::Error + Send + Sync + 'static{
                let r = a.read(b.try_into()?,4)?;
                let r = u32::from_ne_bytes(r.as_ref().as_ref().try_into()?);
                Ok(tuple_list::tuple_list!(r as i32 as $p as $int))
            }
            pub fn [<$p store32>]<T: TryInto<usize>,M: Memory + ?Sized>(a: &mut M, b: T, c: $int) -> anyhow::Result<()> where T::Error: std::error::Error + Send + Sync + 'static{
                // let mut r = &mut a[b.try_into()?..][..4];
                a.write(b.try_into()?,&((c & 0xffffffff) as u32).to_ne_bytes())?;
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
pub fn i64extend132u(a: u32) -> anyhow::Result<tuple_list::tuple_list_type!(u64)>{
    Ok(tuple_list::tuple_list!(a as u64))
}