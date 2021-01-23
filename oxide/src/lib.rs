#![feature(allocator_api)]
#![feature(nonnull_slice_from_raw_parts)]

extern crate itertools;

mod mem;
mod runtime;
mod value;
pub mod vm;

pub use runtime::*;
pub use value::{Value, Object};
pub use vm::Vm;
