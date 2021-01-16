mod address;
mod cell;
mod mem;

use crate::gc::mem::Memory;
use std::{
    fmt::{write, Display},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

pub use address::Address;
pub use cell::{Cell, GcObject, ObjectKind};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<Ty> {
    ptr: Address,
    marker: PhantomData<Ty>,
}

impl<Ty: GcObject> Gc<Ty> {
    pub fn new(ptr: Address) -> Self {
        Self {
            ptr,
            marker: PhantomData,
        }
    }

    pub fn ptr(&self) -> Address {
        self.ptr
    }

    pub fn as_ref(&self) -> &Ty {
        self.ptr.into_ref::<Ty>()
    }

    pub fn as_ref_mut(&mut self) -> &mut Ty {
        self.ptr.into_ref_mut::<Ty>()
    }
}

impl<Ty: Display + GcObject> Display for Gc<Ty> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<Ty: GcObject> Deref for Gc<Ty> {
    type Target = Ty;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<Ty: GcObject> DerefMut for Gc<Ty> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_ref_mut()
    }
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum GcError {}

pub struct GcAlloc {
    memory: Memory,
}

impl GcAlloc {
    pub fn new(size: usize) -> Self {
        let memory = Memory::new(size);
        Self { memory }
    }

    pub fn debug_print(&self) {
        self.memory.debug_print_free_list();
    }

    pub fn contains(&self, address: Address) -> bool {
        self.memory.contains(address.0 as *const _)
    }

    pub fn allocate<T: Sized>(&mut self) -> Option<Address> {
        self.alloc(std::mem::size_of::<T>())
    }

    pub fn alloc(&mut self, size: usize) -> Option<Address> {
        self.memory.alloc(size).map(|ptr| Address(ptr as _))
    }

    pub fn dealloc(&mut self, ptr: Address) {
        self.memory.dealloc(ptr.0 as *const _)
    }
}

impl Drop for GcAlloc {
    fn drop(&mut self) {
        std::mem::drop(&mut self.memory);
    }
}
