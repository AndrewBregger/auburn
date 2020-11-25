use crate::vm::{Instruction, OpCode};
use crate::mem::{read_to, FromBytes};
use crate::Value;

use std::fmt::{Display, Formatter};
use std::slice::SliceIndex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::convert::TryInto;

use ordered_float::OrderedFloat;


#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct SectionId(usize);

impl SectionId {
    pub fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(1);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Display for SectionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SectionInstruction {
    byte: usize,
    instruction: Instruction,
}

impl SectionInstruction {
    fn new(byte: usize, instruction: Instruction) -> Self {
        Self {
            byte,
            instruction
        }
    }

    pub fn byte(&self) -> usize {
        self.byte
    }

    pub fn instruction(&self) -> Instruction {
        self.instruction.clone()
    }
}

impl Display for SectionInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#08x} {}", self.byte(), self.instruction())
    }
}

/// section of executable byte code.
#[derive(Debug, Clone, Default)]
pub struct Section {
    // I am wondering if it would denifit from storing a reference to a array of
    // bytes instead of owning it. This would mean that the section is not able
    // to write to its data since it is owned outside of it.
    data: Vec<u8>,
    id: SectionId,
    constants: Vec<Value>,
    globals: Vec<Value>,
}

impl Section {
    pub fn new() -> Self {
        Self {
            data: vec![],
            id: SectionId::next(),
            constants: vec![],
            globals: vec![],
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }

    pub fn add_global(&mut self, value: Value) -> usize {
        self.globals.push(value);
        self.globals.len() - 1
    }

    pub fn get_global(&self, index: usize) -> Value {
        self.globals[index].clone()
    }

    pub fn id(&self) -> SectionId {
        self.id
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.data.push(byte);
    }

    pub fn write_constant(&mut self, op: OpCode, bytes: &[u8]) {
        self.write_op(op);
        self.write_bytes(bytes);
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.data.push(op as u8);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes)
    }

    /// writes a new label and returns the opcode index after the label.
    pub fn write_label(&mut self, bytes: &str) -> usize {
        // let len: u8 = u8::try_from(bytes.len()).expect("label is too long");
        let len = bytes.len().try_into().expect("label is too long");
        self.write_op(OpCode::Label);
        self.write_byte(len);
        self.write_bytes(bytes.as_bytes());
        self.data.len()
    }

    pub fn read<I>(&self, index: I) -> Option<&<I as SliceIndex<[u8]>>::Output>
    where
        I: SliceIndex<[u8]>,
    {
        self.data.get(index)
    }

    pub unsafe fn read_unchecked<I>(&self, index: I) -> &<I as SliceIndex<[u8]>>::Output
    where
        I: SliceIndex<[u8]>,
    {
        self.data.get_unchecked(index)
    }

    pub fn debug_print(&self) {
        println!("{:#x?}", self.data);
    }

    pub fn disassemble(&self) -> Vec<SectionInstruction> {
        let mut ip = 0;
        let mut res = vec![];
        while ip < self.len() {
            let op_code_raw = unsafe { self.read_unchecked(ip) };
            ip += 1;
            let op_code = OpCode::from_u8(*op_code_raw).unwrap();
            match op_code {
                OpCode::LoadI8 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<i8>(data);
                    let inst = Instruction::LoadI8(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 1;
                }
                OpCode::LoadI16 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<i16>(data);
                    let inst = Instruction::LoadI16(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 2;
                }
                OpCode::LoadI32 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<i32>(data);
                    let inst = Instruction::LoadI32(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 4;
                }
                OpCode::LoadI64 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<i64>(data);
                    let inst = Instruction::LoadI64(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 8;
                }

                OpCode::LoadU8 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u8>(data);
                    let inst = Instruction::LoadU8(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 1;
                }
                OpCode::LoadU16 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u16>(data);
                    let inst = Instruction::LoadU16(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 2;
                }
                OpCode::LoadU32 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    let inst = Instruction::LoadU32(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 4;
                }
                OpCode::LoadU64 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u64>(data);
                    let inst = Instruction::LoadU64(value);
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 8;
                }
                OpCode::LoadF32 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<f32>(data);
                    let inst = Instruction::LoadF32(OrderedFloat::from(value));
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 4;
                }
                OpCode::LoadF64 => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<f64>(data);
                    let inst = Instruction::LoadF64(OrderedFloat::from(value));
                    res.push(SectionInstruction::new(
                        ip - 1,
                        inst
                    ));
                    ip += 8;
                }
                OpCode::Label => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let len = read_to::<u8>(data);
                    let start = ip;
                    ip += 1;
                    let data = &data[1..1 + len as usize];
                    let label = std::str::from_utf8(data).expect("expecting utf8 string");
                    let inst = Instruction::Label(label.to_owned());
                    res.push(SectionInstruction::new(
                        start - 1,
                        inst,
                    ));
                    ip += len as usize;
                }
                OpCode::LoadGlobal => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    let inst = Instruction::LoadGlobal(value);
                    res.push(SectionInstruction::new(ip - 1, inst));
                    ip += 4;
                }
                OpCode::JmpTrue => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    let inst = Instruction::JmpTrue(value);
                    res.push(SectionInstruction::new(ip - 1, inst));
                    ip += 4;
                }
                OpCode::JmpFalse => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    let inst = Instruction::JmpFalse(value);
                    res.push(SectionInstruction::new(ip - 1, inst));
                    ip += 4;
                }
                OpCode::Jmp => {
                    let data = self.read(ip..).expect("unable to read buffer");
                    let value = read_to::<u32>(data);
                    let inst = Instruction::Jmp(value);
                    res.push(SectionInstruction::new(ip - 1, inst));
                    ip += 4;
                }
                OpCode::Exit
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
                | OpCode::Print => {
                    res.push(SectionInstruction::new(
                        ip - 1,
                        Instruction::from(op_code)
                    ));
                }
                OpCode::Call | _ => {}
            }
        }
        res
    }
}
