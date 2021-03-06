#![feature(allocator_api)]
#![feature(nonnull_slice_from_raw_parts)]
#![feature(backtrace)]

extern crate itertools;

mod mem;
mod runtime;
mod value;
pub mod vm;

pub use runtime::*;
pub use value::Value;
pub use vm::Vm;

