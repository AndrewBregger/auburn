use std::{
    cmp::min,
    fmt::{Display, Formatter},
};

use crate::gc::{Cell, Gc, GcObject};

use super::ArrayBuffer;

#[derive(Debug, Clone, Copy)]
pub struct OxString {
    len: usize,
    buffer: Gc<ArrayBuffer<char>>,
}

impl OxString {
    pub fn new(buffer: Gc<ArrayBuffer<char>>, len: usize, cap: usize) -> Self {
        Self { len, buffer }
    }

    pub fn chars(&self) -> &[char] {
        self.buffer.as_slice()
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    // naive, only store what it can in the buffer.
    pub fn set_from_str(&mut self, value: &str) {
        let buffer = self.buffer.as_slice_mut();
        let len = min(value.len(), buffer.len());
        for (idx, value) in value.chars().take(len).enumerate() {
            buffer[idx] = value;
        }
    }
}

impl Display for OxString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // @TODO(): fix display to properly display a oxide string([char])
        for char in self.chars() {
            write!(f, "{}", *char)?;
        }
        Ok(())
    }
}

impl GcObject for OxString {
    fn as_cell(&self) -> &Cell {
        self.buffer.as_cell()
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        self.buffer.as_cell_mut()
    }
}
