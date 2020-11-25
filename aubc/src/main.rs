extern crate auburn;

use std::error::Error;

use crate::core::Core;
use auburn::oxide::vm::{OpCode, Vm};
use auburn::oxide::{Section, Value};

mod core;

fn main() -> Result<(), Box<dyn Error>> {
    // Core::new().run();
    println!("Max: {}", OpCode::NumOps as u8);
    let mut section = Section::new();
    let x = section.add_global(Value::F32(1.0f32));
    let y = section.add_global(Value::F32(2.4f32));

    let l2 = section.write_label("l2");
    section.write_constant(OpCode::LoadGlobal, &(y as u32).to_be_bytes());
    section.write_constant(OpCode::LoadF32, &3.5f32.to_be_bytes());
    section.write_op(OpCode::MultF32);
    section.write_op(OpCode::Print);
    section.write_op(OpCode::Pop);
    section.write_op(OpCode::Exit);
    let l1 = section.write_label("l1");
    section.write_constant(OpCode::LoadGlobal, &(x as u32).to_be_bytes());
    section.write_op(OpCode::Print);
    section.write_constant(OpCode::LoadF32, &4.3f32.to_be_bytes());
    section.write_op(OpCode::LessF32);
    section.write_constant(OpCode::JmpTrue, &(l2 as u32).to_be_bytes());
    // let l3 = section.write_label("l3");
    // println!("{} {} {}", l1, l2, l3);

    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&0.5f32.to_be_bytes());
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&0.5f32.to_be_bytes());
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&0.5f32.to_be_bytes());
    // section.write_op(OpCode::MultF32);
    // section.write_op(OpCode::MultF32);
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&1.4f32.to_be_bytes());
    // section.write_op(OpCode::AddF32);
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&1.0f32.to_be_bytes());
    // section.write_op(OpCode::LessF32);
    // section.write_op(OpCode::Print);
    // section.write_op(OpCode::Pop);
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&0.5f32.to_be_bytes());
    // section.write_op(OpCode::LoadF32);
    // section.write_bytes(&0.5f32.to_be_bytes());
    // section.write_op(OpCode::LessEqF32);
    // section.write_op(OpCode::Print);
    section.debug_print();
    let disassemble = section.disassemble();
    for inst in disassemble {
        println!("{}", inst);
    }
    let mut vm = Vm::new();
    vm.run_from(&section, l1)?;
    Ok(())
}
