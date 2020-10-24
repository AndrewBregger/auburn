extern crate language;

mod core;

use crate::core::Core;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    Core::new().run();
    Ok(())
}
