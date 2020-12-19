use crate::mem::{read_to, FromBytes};
use crate::vm::{Instruction, OpCode};
use crate::Value;

use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::slice::SliceIndex;
use std::sync::atomic::{AtomicUsize, Ordering};

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

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1).try_into().unwrap()
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }

    pub fn add_global(&mut self, value: Value) -> u8 {
        self.globals.push(value);
        (self.globals.len() - 1).try_into().unwrap()
    }

    pub fn get_global(&self, index: usize) -> Value {
        self.globals[index].clone()
    }

    pub fn set_global(&mut self, index: usize, value: Value) {
        self.globals[index] = value;
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

    pub fn write_load(&mut self, op: OpCode, index: u8) {
        self.write_op(op);
        self.write_byte(index);
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.data.push(op as u8);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes)
    }

    // returns the first byte of the jmp operand.
    pub fn write_jmp(&mut self, op: OpCode) -> usize {
        self.write_op(op);
        self.write_bytes(&[0xff, 0xff]);
        self.len() - 2
    }

    pub fn patch_jmp(&mut self, offset: usize) {
        println!("patch_jmp: {} {}", self.len(), offset);
        let jump: u16 = (self.len() - offset - 2)
            .try_into()
            .expect("attempting to jump too far");
        self.data[offset] = ((jump >> 8) & 0xff) as u8;
        self.data[offset + 1] = (jump & 0xff) as u8;
    }

    pub fn write_loop(&mut self, start: usize) {
        self.write_op(OpCode::Loop);
        let offset = (self.len() - start + 2) as u16;
        self.write_bytes(&offset.to_be_bytes());
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

    pub fn data(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn debug_print(&self) {
        println!("{:#?}", self.data);
    }

    pub fn disassemble(&self) -> Vec<Instruction> {
        let mut ip = 0;
        let mut res = vec![];
        while ip < self.len() {
            let op_code_raw = unsafe { self.read_unchecked(ip) };
            ip += 1;
            let op_code = OpCode::from_u8(*op_code_raw).unwrap();
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
                | OpCode::LoadGlobal
                | OpCode::SetGlobal
                | OpCode::LoadLocal
                | OpCode::SetLocal
                | OpCode::Call => {
                    let value = *unsafe { self.read_unchecked(ip) };
                    res.push(Instruction::with_arg(ip - 1, op_code, value as u16));
                    ip += 1;
                }
                OpCode::Loop | OpCode::JmpTrue | OpCode::JmpFalse | OpCode::Jmp => {
                    let offset = ip - 1;
                    let value = read_to::<u16>(self.data(), &mut ip);
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
                | OpCode::Print => {
                    res.push(Instruction::simple(ip - 1, op_code));
                }
                OpCode::NumOps => {}
            }
        }
        res
    }

    pub fn globals(&self) -> &[Value] {
        self.globals.as_slice()
    }

    pub fn constants(&self) -> &[Value] {
        self.constants.as_slice()
    }
}
