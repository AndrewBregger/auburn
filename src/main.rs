extern crate language;

mod core;

use crate::core::{Core, CoreError};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut core = Core::new();
    core.run();
    Ok(())
}
