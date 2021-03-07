use std::convert::TryInto;

pub trait FromBytes {
    fn from_bytes(bytes: &[u8], len: usize) -> Self;
}

macro_rules! from_bytes {
    ($T:ty) => {
        impl FromBytes for $T {
            fn from_bytes(bytes: &[u8], len: usize) -> Self {
                Self::from_be_bytes(
                    (&bytes[0..len])
                        .try_into()
                        .expect("invalid length for slice"),
                )
            }
        }
    };
}

from_bytes!(i8);
from_bytes!(i16);
from_bytes!(i32);
from_bytes!(i64);

from_bytes!(u8);
from_bytes!(u16);
from_bytes!(u32);
from_bytes!(u64);

from_bytes!(f32);
from_bytes!(f64);

pub fn read_to<T: FromBytes>(data: &[u8], offset: &mut usize) -> T {
    // implementation should be change to not use &[u8] since that requires constructing it from raw
    let size = std::mem::size_of::<T>();
    let value = T::from_bytes(&data[*offset..], size);
    *offset += size;
    value
}
