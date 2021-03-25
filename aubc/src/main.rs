#![feature(allocator_api)]
#![feature(backtrace)]

extern crate auburn;

use crate::core::Core;
use std::error::Error;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    Core::new().run();
    Ok(())
}
