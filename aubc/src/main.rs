#![feature(allocator_api)]
extern crate auburn;

use std::error::Error;
use crate::core::Core;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    Core::new().run();
    Ok(())
}
