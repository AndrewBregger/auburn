mod address;
mod cell;
mod mem;
mod object;

use std::{
    alloc::Layout,
    fmt::Display,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

pub use address::Address;
pub use cell::Cell;
pub use mem::{Arena, Header, Memory, Pool, VecAllocator};
pub use object::*;

use self::mem::AllocationRecord;

#[repr(transparent)]
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<Ty> {
    ptr: Address,
    marker: PhantomData<Ty>,
}

impl<Ty> Clone for Gc<Ty> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }
}

impl<Ty> Copy for Gc<Ty> {}

impl<Ty> Gc<Ty> {
    pub fn new(ptr: Address) -> Self {
        Self {
            ptr,
            marker: PhantomData,
        }
    }

    pub fn with_value(address: Address, value: Ty) -> Self {
        unsafe {
            std::ptr::write(address.as_ptr_mut() as *mut Ty, value);
        }
        Gc::new(address)
    }

    pub fn null() -> Self {
        Self::new(Address::null())
    }

    pub fn ptr(&self) -> Address {
        self.ptr
    }
}

impl<Ty: Object> Gc<Ty> {
    pub fn as_ref(&self) -> &Ty {
        self.ptr.into_ref::<Ty>()
    }

    pub fn as_ref_mut(&mut self) -> &mut Ty {
        self.ptr.into_ref_mut::<Ty>()
    }
}

impl<Ty: Display + Object> Display for Gc<Ty> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<Ty: Object> Deref for Gc<Ty> {
    type Target = Ty;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<Ty: Object> DerefMut for Gc<Ty> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_ref_mut()
    }
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum GcError {}

pub struct GcAlloc {
    memory: Arc<Mutex<Memory>>,
}

impl GcAlloc {
    pub fn new() -> Self {
        let memory = Memory::new();
        Self {
            memory: Arc::new(Mutex::new(memory)),
        }
    }

    pub fn memory_usage(&self) -> usize {
        self.memory
            .lock()
            .expect("failed to retreive memory lock")
            .memory_usage()
    }

    pub fn should_collect(&self) -> bool {
        self.memory
            .lock()
            .expect("failed to retreive memory lock")
            .should_collect()
    }

    #[inline(always)]
    pub fn allocator_vec(&self) -> VecAllocator {
        VecAllocator::new(self.memory.clone())
    }

    #[inline(always)]
    pub fn allocate<T: Object + Sized>(&mut self) -> Option<Address> {
        let layout = Layout::new::<T>();
        self.alloc(T::object_kind(), layout)
    }

    #[inline(always)]
    pub fn alloc(&mut self, kind: ObjectKind, layout: Layout) -> Option<Address> {
        let address = self
            .memory
            .lock()
            .expect("failed to retrieve memory lock")
            .alloc_inner(kind, layout)
            .map(|ptr| Address::from_ptr(ptr.as_ptr() as *mut u8))
            .ok();

        address
    }

    #[inline(always)]
    pub fn dealloc(&mut self, ptr: Address) {
        self.dealloc_inner(ptr)
    }

    #[inline(always)]
    pub fn dealloc_inner(&mut self, ptr: Address) {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .dealloc(ptr.as_ptr_mut())
    }

    #[cfg(debug_assertions)]
    pub fn last_record(&self) -> Option<AllocationRecord> {
        self.memory
            .lock()
            .expect("failed to retreive memory lock")
            .last_record()
            .map(Clone::clone)
    }

    #[cfg(debug_assertions)]
    pub fn records(&self) -> Vec<AllocationRecord> {
        self.memory
            .lock()
            .expect("failed to retreive memory lock")
            .records()
            .to_vec()
    }

    pub fn dump_mem_stats(&self) {
        self.memory
            .lock()
            .expect("failed to retreive memory lock")
            .dump_mem_stats();
        println!("Sum: {}", self.memory_usage());
    }
}
