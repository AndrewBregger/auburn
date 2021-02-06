use std::{
    cmp::min,
    fmt::{Display, Formatter},
    ops::DerefMut,
};

use crate::{
    gc::{Address, Cell, Gc, GcObject},
    VecBuffer,
};

#[derive(Debug, Clone)]
pub struct OxString {
    len: usize,
    buffer: VecBuffer<char>,
}

impl OxString {
    pub fn new(buffer: VecBuffer<char>, len: usize) -> Self {
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
        for value in value.chars() {
            self.buffer.push(value);
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
