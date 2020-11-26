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
    let x = section.add_global(Value::I32(0));
    let end = section.add_constant(Value::I32(10));
    let dec = section.add_constant(Value::I32(1));

    // while x < 10 {
    //     print(x);
    //     x = x + 1;
    // }
    let start = section.len();
    section.write_load(OpCode::LoadGlobal, x);
    section.write_load(OpCode::LoadI32, end);
    section.write_op(OpCode::LessI32);
    let while_exit = section.write_jmp(OpCode::JmpFalse);
    section.write_op(OpCode::Pop);
    section.write_load(OpCode::LoadGlobal, x);
    section.write_op(OpCode::Print);
    // section.write_load(OpCode::LoadGlobal, x);
    section.write_load(OpCode::LoadI32, dec);
    section.write_op(OpCode::AddI32);
    section.write_load(OpCode::SetGlobal, x);
    section.write_loop(start);
    section.patch_jmp(while_exit);
    section.write_op(OpCode::Exit);

    section.debug_print();
    let disassemble = section.disassemble();
    for inst in disassemble {
        println!("{}", inst);
    }
    let mut vm = Vm::new();
    vm.run(&mut section)?;
    Ok(())
}
