use std::{cmp::min, fmt::{Display, Formatter}, ops::{Deref, DerefMut}};

use crate::{VecBuffer, gc::{Address, Cell, Gc, GcObject, ObjectKind}};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct OxString {
    cell: Cell,
    pub buffer: VecBuffer<char>,
    pub len: usize,
}

impl OxString {
    pub fn new(buffer: VecBuffer<char>, len: usize) -> Self {
        Self { 
            cell: Cell::new(ObjectKind::String),
            len,
            buffer 
        }
    }

    pub fn chars(&self) -> &[char] {
        self.buffer.deref().as_slice()
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
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}
