extern crate auburn;

use std::error::Error;

use crate::core::Core;
use auburn::oxide::vm::{OpCode, Vm};
use auburn::oxide::Section;

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    // Core::new().run();
    println!("Max: {}", OpCode::NumOps as u8);
    let mut section = Section::new();
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&0.5f32.to_be_bytes());
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&0.5f32.to_be_bytes());
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&0.5f32.to_be_bytes());
    section.write_op(OpCode::MultF32);
    section.write_op(OpCode::MultF32);
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&1.4f32.to_be_bytes());
    section.write_op(OpCode::AddF32);
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&1.0f32.to_be_bytes());
    section.write_op(OpCode::LessF32);
    section.write_op(OpCode::Print);
    section.write_op(OpCode::Pop);
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&0.5f32.to_be_bytes());
    section.write_op(OpCode::LoadF32);
    section.write_bytes(&0.5f32.to_be_bytes());
    section.write_op(OpCode::LessEqF32);
    section.write_op(OpCode::Print);
    section.debug_print();
    let mut vm = Vm::new();
    vm.run(&section)?;
    Ok(())
}
