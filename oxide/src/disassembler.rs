use crate::{
    mem::read_to,
    vm::{Instruction, OpCode},
    Section,
};

pub struct Disassembler;

impl Disassembler {
    pub fn disassemble_section(section: &Section) -> Vec<Instruction> {
        let mut ip = 0;
        let mut res = vec![];
        while ip < section.len() {
            let op_code_raw = section.read(ip);
            ip += 1;
            let op_code = OpCode::from_u8(op_code_raw).unwrap();
            // println!("OpCode {}", op_code);
            match op_code {
                OpCode::LoadI8
                | OpCode::LoadI16
                | OpCode::LoadI32
                | OpCode::LoadI64
                | OpCode::LoadU8
                | OpCode::LoadU16
                | OpCode::LoadU32
                | OpCode::LoadU64
                | OpCode::LoadF32
                | OpCode::LoadF64
                | OpCode::LoadStr
                | OpCode::LoadChar => {
                    let value = section.read(ip);
                    let con = section.get_constant(value as usize);
                    res.push(Instruction::with_arg_and_const(
                        ip - 1,
                        op_code,
                        value as u16,
                        con,
                    ));
                    ip += 1;
                }
                OpCode::LoadGlobal => {
                    let value = section.read(ip);
                    let con = section.get_global(value as usize);
                    res.push(Instruction::with_arg_and_const(
                        ip - 1,
                        op_code,
                        value as u16,
                        con,
                    ));
                    ip += 1;
                }
                OpCode::SetGlobal
                | OpCode::LoadLocal
                | OpCode::SetLocal
                | OpCode::SetRegister
                | OpCode::SetAttr
                | OpCode::LoadRegister
                | OpCode::Call => {
                    let value = section.read(ip);
                    res.push(Instruction::with_arg(ip - 1, op_code, value as u16));
                    ip += 1;
                }
                OpCode::Loop
                | OpCode::JmpTrue
                | OpCode::JmpFalse
                | OpCode::Jmp
                | OpCode::NewInstance
                | OpCode::NewTuple
                | OpCode::InstanceAttr
                | OpCode::TupleAttr => {
                    let offset = ip - 1;
                    let value = read_to::<u16>(section.data(), &mut ip);
                    res.push(Instruction::with_arg(offset, op_code, value as u16));
                }
                OpCode::Label => {}
                OpCode::Return
                | OpCode::Exit
                | OpCode::LoadTrue
                | OpCode::LoadFalse
                | OpCode::AddI8
                | OpCode::AddI16
                | OpCode::AddI32
                | OpCode::AddI64
                | OpCode::AddU8
                | OpCode::AddU16
                | OpCode::AddU32
                | OpCode::AddU64
                | OpCode::AddF32
                | OpCode::AddF64
                | OpCode::SubI8
                | OpCode::SubI16
                | OpCode::SubI32
                | OpCode::SubI64
                | OpCode::SubU8
                | OpCode::SubU16
                | OpCode::SubU32
                | OpCode::SubU64
                | OpCode::SubF32
                | OpCode::SubF64
                | OpCode::MultI8
                | OpCode::MultI16
                | OpCode::MultI32
                | OpCode::MultI64
                | OpCode::MultU8
                | OpCode::MultU16
                | OpCode::MultU32
                | OpCode::MultU64
                | OpCode::MultF32
                | OpCode::MultF64
                | OpCode::DivI8
                | OpCode::DivI16
                | OpCode::DivI32
                | OpCode::DivI64
                | OpCode::DivU8
                | OpCode::DivU16
                | OpCode::DivU32
                | OpCode::DivU64
                | OpCode::DivF32
                | OpCode::DivF64
                | OpCode::LessI8
                | OpCode::LessI16
                | OpCode::LessI32
                | OpCode::LessI64
                | OpCode::LessU8
                | OpCode::LessU16
                | OpCode::LessU32
                | OpCode::LessU64
                | OpCode::LessF32
                | OpCode::LessF64
                | OpCode::GreaterI8
                | OpCode::GreaterI16
                | OpCode::GreaterI32
                | OpCode::GreaterI64
                | OpCode::GreaterU8
                | OpCode::GreaterU16
                | OpCode::GreaterU32
                | OpCode::GreaterU64
                | OpCode::GreaterF32
                | OpCode::GreaterF64
                | OpCode::LessEqI8
                | OpCode::LessEqI16
                | OpCode::LessEqI32
                | OpCode::LessEqI64
                | OpCode::LessEqU8
                | OpCode::LessEqU16
                | OpCode::LessEqU32
                | OpCode::LessEqU64
                | OpCode::LessEqF32
                | OpCode::LessEqF64
                | OpCode::GreaterEqI8
                | OpCode::GreaterEqI16
                | OpCode::GreaterEqI32
                | OpCode::GreaterEqI64
                | OpCode::GreaterEqU8
                | OpCode::GreaterEqU16
                | OpCode::GreaterEqU32
                | OpCode::GreaterEqU64
                | OpCode::GreaterEqF32
                | OpCode::GreaterEqF64
                | OpCode::Pop
                | OpCode::FrameStack
                | OpCode::PushLocal
                | OpCode::Echo => {
                    res.push(Instruction::simple(ip - 1, op_code));
                }
                OpCode::NumOps => {}
            }
        }
        res
    }
}
