#![feature(allocator_api)]
#![feature(backtrace)]

extern crate auburn;

use crate::core::Core;
use std::error::Error;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    Core::new().run();
    
//    let mut vm = Vm::new();

//    let s = vm.allocate_string("value");
//    let cell_ptr = s.as_cell() as *const Cell as *const u8 as usize;
//    let s_ptr = (&s) as *const OxString as *const u8 as usize;
//
//    println!("diff: {}", cell_ptr - s_ptr);
    
//    let mut objects = vec![];
//
//    for i in 0..10 {
//        let s = vm.allocate_string_ptr(format!("test{}", i).as_str());
//        objects.push(Value::from(s));
//    }
//    
//    let name = vm.allocate_string("fn_test");
//    let section = Section::new(&mut vm);
//    let function = vm.allocate_function(name, 0, section);
//
//    objects.push(Value::from(function));
//    
//    for obj in objects.into_iter() {
//        vm.push_stack(obj);
//    }
//
//    println!("Allocated Size: {}", vm.memory_usage());
//
//    vm.collect();
//
//    println!("Allocated Size: {}", vm.memory_usage());



    Ok(())
}
