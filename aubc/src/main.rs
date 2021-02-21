#![feature(allocator_api)]
extern crate auburn;

use crate::core::Core;
use std::error::Error;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    Core::new().run();
    Ok(())
}
