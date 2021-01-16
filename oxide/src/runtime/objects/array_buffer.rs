use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::gc::{Cell, GcObject};

use super::Buffer;

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct ArrayBuffer<Ty> {
    buffer: Buffer,
    marker: PhantomData<Ty>,
}

impl<Ty> ArrayBuffer<Ty> {
    pub fn new(buffer: Buffer) -> Self {
        Self {
            buffer,
            marker: PhantomData,
        }
    }

    pub fn ptr(&self) -> *const Ty {
        self.buffer.ptr() as *const Ty
    }

    pub fn ptr_mut(&mut self) -> *mut Ty {
        self.buffer.ptr_mut() as *mut Ty
    }

    pub fn as_slice_mut(&mut self) -> &mut [Ty] {
        println!("Arraybuffer::len {}", self.len());
        unsafe { std::slice::from_raw_parts_mut(self.ptr_mut(), self.len()) }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.buffer.len() / std::mem::size_of::<Ty>()
    }

    pub fn as_slice(&self) -> &[Ty] {
        unsafe { std::slice::from_raw_parts(self.buffer.ptr() as *const Ty, self.len()) }
    }
}

impl<Ty> Index<usize> for ArrayBuffer<Ty> {
    type Output = Ty;

    fn index(&self, index: usize) -> &Self::Output {
        let typed_buffer_size = self.len();
        if index < typed_buffer_size {
            panic!(
                "index {} is out of bound for buffer of size {}",
                index, typed_buffer_size
            );
        }

        let ptr = self.buffer.ptr() as *mut Ty;
        unsafe { &*ptr.add(index) }
    }
}

impl<Ty> IndexMut<usize> for ArrayBuffer<Ty> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let typed_buffer_size = self.len();
        if index < typed_buffer_size {
            panic!(
                "index {} is out of bound for buffer of size {}",
                index, typed_buffer_size
            );
        }

        let ptr = self.buffer.ptr() as *mut Ty;
        unsafe { &mut *ptr.add(index) }
    }
}

impl<Ty: Sync + Send + Copy> GcObject for ArrayBuffer<Ty> {
    fn as_cell(&self) -> &Cell {
        self.buffer.as_cell()
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        self.buffer.as_cell_mut()
    }
}
