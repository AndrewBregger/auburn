#![feature(allocator_api)]
#![feature(nonnull_slice_from_raw_parts)]
#![feature(backtrace)]

extern crate itertools;

mod disassembler;
mod mem;
mod runtime;
mod value;
mod bit_map;
pub mod vm;

pub use runtime::*;
pub use value::Value;
pub use vm::Vm;
pub use bit_map::BitMap;
