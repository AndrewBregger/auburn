use crate::gc::{Cell, GcObject, ObjectKind};

// this is really a buffer header.
#[derive(Debug, Clone, Copy)]
pub struct Buffer {
    pub cell: Cell,
    pub size: usize,
}

impl Buffer {
    pub fn new(size: usize) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Buffer),
            size,
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.size
    }

    pub fn ptr(&self) -> *const u8 {
        let ptr = self as *const Buffer as *const u8;
        unsafe { ptr.add(std::mem::size_of::<Cell>() + std::mem::size_of::<usize>()) }
    }

    pub fn ptr_mut(&mut self) -> *mut u8 {
        let ptr = self as *mut Buffer as *mut u8;
        unsafe { ptr.add(std::mem::size_of::<Cell>() + std::mem::size_of::<usize>()) }
    }
}

// unsafe impl Send for Buffer {}
// unsafe impl Sync for Buffer {}

impl GcObject for Buffer {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}
