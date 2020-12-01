extern crate auburn;

use std::error::Error;

use crate::core::Core;
use auburn::oxide::vm::{OpCode, Vm};
use auburn::oxide::{OxFunction, OxString};
use auburn::oxide::{Section, Value};

mod core;

pub fn create_while_test(section: &mut Section) {
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
}

fn create_string_test(section: &mut Section) {
    let x = section.add_constant(Value::String(Box::new(OxString::from("hello, Oxide"))));
    section.write_load(OpCode::LoadStr, x);
    section.write_op(OpCode::Print);
    section.write_op(OpCode::Exit);
}

fn build_function_add() -> Box<OxFunction> {
    let mut section = Section::new();
    // let x = section.add_constant(Value::F32(10.0f32));
    // let y = section.add_constant(Value::F32(2.4f32));

    section.write_load(OpCode::LoadLocal, 0);
    section.write_load(OpCode::LoadLocal, 1);
    section.write_op(OpCode::MultF32);
    section.write_op(OpCode::Return);
    // section.write_op(OpCode::Exit);

    Box::new(OxFunction::new(
        Box::new(OxString::from("add")),
        2,
        section,
    ))
}

fn build_script() -> Box<OxFunction> {
    let add_function = build_function_add();
    let mut section = Section::new();
    let add = section.add_global(Value::Function(add_function));
    section.add_constant(Value::F32(3.69));
    section.add_constant(Value::F32(33.31));

    section.write_load(OpCode::LoadGlobal, add);
    section.write_load(OpCode::LoadF32, 0);
    section.write_load(OpCode::LoadF32, 1);
    section.write_load(OpCode::Call, 2);
    section.write_op(OpCode::Print);
    section.write_op(OpCode::Pop);
    section.write_op(OpCode::Exit);

    Box::new(OxFunction::new(
        Box::new(OxString::from("")),
        0,
        section,
    ))
}

fn main() -> Result<(), Box<dyn Error>> {
    // Core::new().run();
    println!("Max: {}", OpCode::NumOps as u8);
    let mut vm = Vm::new();
    let funct = build_script();
    vm.run(funct)?;
    Ok(())
}
