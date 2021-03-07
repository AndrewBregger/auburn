mod address;
mod cell;
mod mem;

pub use mem::{Allocator, Header, Memory, VecAllocator};
use std::{
    alloc::Layout,
    fmt::Display,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

pub use address::Address;
pub use cell::{Cell, GcObject, ObjectKind};


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

impl<Ty: GcObject> Gc<Ty> {
    pub fn new(ptr: Address) -> Self {
        Self {
            ptr,
            marker: PhantomData,
        }
    }

    pub fn null() -> Self {
        Self::new(Address::null())
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
    memory: Arc<Mutex<Memory>>,
}

impl GcAlloc {
    pub fn new(size: usize) -> Self {
        let memory = Memory::new(size);
        Self {
            memory: Arc::new(Mutex::new(memory)),
        }
    }

    pub fn sweep(&mut self) {
        self.memory.lock().expect("failed to retreive memory lock").sweep();
    }

    pub fn memory_usage(&self) -> usize {
        self.memory.lock().expect("failed to retreive memory lock").memory_usage()
    }

    pub fn should_collect(&self) -> bool {
        self.memory.lock().expect("failed to retreive memory lock").should_collect()
    }

    pub fn post_collect(&mut self) {
        self.memory.lock().expect("failed to retreive memory lock").post_collect();
    }

    pub fn allocator(&self) -> Allocator {
        Allocator::new(self.memory.clone())
    }

    pub fn allocator_vec(&self) -> VecAllocator {
        VecAllocator::new(self.memory.clone())
    }

    pub fn debug_print(&self) {
        self.memory
            .lock()
            .expect("faild to retrieve memory lock")
            .debug_print_free_list();
    }

    pub fn contains(&self, address: Address) -> bool {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .contains(address.as_ptr())
    }

    pub fn allocate<T: Sized>(&mut self) -> Option<Address> {
        // safetly: This layout is being generated from the size and alignment of the type itself.
        // This is coming from the compiler so it shouldn't have to be checked again.
        let layout = unsafe {
            Layout::from_size_align_unchecked(std::mem::size_of::<T>(), std::mem::align_of::<T>())
        };
        self.alloc(layout)
    }

    pub fn alloc(&mut self, layout: Layout) -> Option<Address> {
        let address = self
            .memory
            .lock()
            .expect("failed to retrieve memory lock")
            .alloc_inner(layout)
            .map(|ptr| Address::from_ptr(ptr.as_ptr() as *mut u8))
            .ok();

        address
    }

    pub fn dealloc(&mut self, ptr: Address) {
        self.dealloc_inner(ptr)
    }

    pub fn dealloc_inner(&mut self, ptr: Address) {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .dealloc(ptr.as_ptr_mut())
    }

    pub fn clean_up(&mut self) {
        self.memory.lock().expect("failed to retreive memory lock").clean_up();
    }
}

impl Drop for GcAlloc {
    fn drop(&mut self) {
        std::mem::drop(&mut self.memory);
    }
}
