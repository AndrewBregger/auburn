extern crate auburn;

use std::{alloc::Layout, error::Error};

use auburn::oxide::{
    gc::{Address, Cell, Gc, GcAlloc, GcObject},
    Vm,
};

use crate::core::Core;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    // Core::new().run();

    let mut vm = Vm::new();
    let ox_string = vm.allocate_string("hello, world");
    println!("{:?}", ox_string);
    println!("chars: {:?}", ox_string.chars());
    println!("{}", ox_string);

    Ok(())
}
