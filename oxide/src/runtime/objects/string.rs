use std::fmt::Display;

use crate::{
    gc::{Object, ObjectKind, VecAllocator},
    OxVec,
};

#[derive(Debug, Clone)]
pub struct OxString {
    buffer: OxVec<u8>,
}

impl OxString {
    pub fn new(allocator: VecAllocator) -> Self {
        Self {
            buffer: OxVec::new(allocator),
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.buffer.as_slice()) }
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn buffer(&self) -> &OxVec<u8> {
        &self.buffer
    }

    pub fn capacity(&self) -> usize {
        self.buffer.capacity()
    }

    pub fn with_value(allocator: VecAllocator, value: &str) -> Self {
        let mut val = Self {
            buffer: OxVec::new(allocator),
        };

        val.push_bytes(value.as_bytes());

        val
    }

    pub fn push_bytes(&mut self, value: &[u8]) {
        self.buffer.extend_from_slice(value)
    }

    pub fn disassemble(&self, indent: usize) {
        println!(
            "{}<string {}>",
            (0..indent).map(|_| '\t').collect::<String>(),
            self
        );
    }
}

impl Display for OxString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Object for OxString {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}
