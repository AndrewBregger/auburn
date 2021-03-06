use std::{
    ops::{Deref, DerefMut, Index, IndexMut},
    slice::SliceIndex,
};

use crate::gc::{Cell, GcObject, VecAllocator};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct VecBuffer<Ty> {
    cell: Cell,
    buffer: Vec<Ty, VecAllocator>,
}

// impl<Ty> Copy for VecBuffer<Ty> {}

impl<Ty> VecBuffer<Ty> {
    pub fn new(buffer: Vec<Ty, VecAllocator>) -> Self {
        Self {
            cell: Cell::new(crate::gc::ObjectKind::VecBuffer),
            buffer,
        }
    }

    pub fn empty(allocator: VecAllocator) -> Self {
        Self::new(Vec::<Ty, VecAllocator>::new_in(allocator))
    }
}

impl<Ty: Send + Sync> GcObject for VecBuffer<Ty> {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}

impl<Ty> Deref for VecBuffer<Ty> {
    type Target = Vec<Ty, VecAllocator>;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl<Ty> DerefMut for VecBuffer<Ty> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer
    }
}

impl<Ty, Idx> Index<Idx> for VecBuffer<Ty>
where
    Idx: SliceIndex<[Ty]>,
{
    type Output = <Idx as SliceIndex<[Ty]>>::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        self.buffer.index(index)
    }
}

impl<Ty, Idx> IndexMut<Idx> for VecBuffer<Ty>
where
    Idx: SliceIndex<[Ty]>,
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        self.buffer.index_mut(index)
    }
}
