use std::{alloc::{AllocError, Layout}, collections::BTreeSet, sync::atomic::{AtomicUsize, Ordering}};
use std::{
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use itertools::Itertools;

// pub static ALIGN_HEADER: usize = std::mem::align_of::<Header>();
// pub static ALIGN_LIST_NODE: usize = std::mem::align_of::<FreeListNode>();
static COLLECT_INITIAL: usize = 64; //1024 * 1024;
static COLLECT_FACTOR: f64 = 1.5;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Header {
    pub id: usize,
    pub size: usize,
    pub align: usize,
}

impl From<Layout> for Header {
    fn from(other: Layout) -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: TOKEN.fetch_add(1, Ordering::SeqCst),
            size: other.size(),
            align: other.align(),
        }
    }
}

#[derive(Debug)]
pub struct Memory {
    layout: Layout,
    size: usize,
    memory_usage: usize,
    next_collect: usize,
    allocations: BTreeSet<usize>,
}

impl Memory {
    pub fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, 1)
            .expect(format!("failed to allocate Arena({})", size).as_str());

        Self {
            layout,
            size,
            memory_usage: 0,
            next_collect: COLLECT_INITIAL,
            allocations: BTreeSet::new(),
        }
    }

    pub fn memory_usage(&self) -> usize {
        self.memory_usage
    }

    pub fn should_collect(&self) -> bool {
        self.memory_usage >= self.next_collect
    }

    pub fn post_collect(&mut self) {
        self.next_collect = (self.next_collect as f64 * COLLECT_FACTOR) as usize;
    }

    pub fn sweep(&mut self) {
    }


    pub fn contains(&self, ptr: *const u8) -> bool {
        // self.buffer as *const _ <= ptr && ptr < self.end as *const _
        true
    }

    pub unsafe fn get_header(&self, ptr: *const u8) -> &Header {
        let header = ptr.sub(std::mem::size_of::<Header>()) as *const Header;
        header
            .as_ref()
            .expect(format!("should be able to dereference this pointer: {:p}", header).as_str())
    }

    pub fn alloc(&mut self, layout: Layout) -> Option<*mut u8> {
        self.alloc_inner(layout)
            .ok()
            .map(|ptr| ptr.as_ptr() as *mut u8).and_then(|ptr| {
                self.allocations.insert(ptr as _);
                Some(ptr)
            })
    }

    pub fn alloc_inner(&mut self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        println!("allocating: {:?}", layout);
        if layout.size() == 0 {
            return Ok(NonNull::slice_from_raw_parts(NonNull::<u8>::dangling(), 0));
        }

        let layout = Layout::from_size_align(
            layout.size() + std::mem::size_of::<Header>(),
            layout.align().max(std::mem::align_of::<Header>()),
        )
        .expect("failed to layout header");

        self.memory_usage += layout.size();

        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            Err(std::alloc::AllocError)
        } else {
            unsafe {
                let header = &mut *(ptr as *mut Header);
                *header = Header::from(layout);
                let ptr = ptr.add(std::mem::size_of::<Header>());
                println!("IsValid Alignment {}", ptr as usize % layout.align() == 0);
                let ptr = NonNull::new(ptr).ok_or(std::alloc::AllocError)?;
                Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
            }
        }
    }

    pub fn alloc_inner_vec(&mut self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            Err(std::alloc::AllocError)
        } else {
            self.memory_usage += layout.size();
            let nnptr = NonNull::new(ptr).ok_or(std::alloc::AllocError)?;
            self.allocations.insert(ptr as _);
            Ok(NonNull::slice_from_raw_parts(nnptr, layout.size()))
        }
    }

    pub fn dealloc(&mut self, ptr: *mut u8) {
        let header_ptr = unsafe { ptr.sub(std::mem::size_of::<Header>()) } as *mut u8;
        let header = unsafe { &mut *(header_ptr as *mut Header) };
        let layout = match Layout::from_size_align(header.size, std::mem::align_of::<Header>()) {
            Ok(layout) => layout,
            Err(e) => {
                eprintln!("dealloc: err {}", e);
                panic!();
            }
        };
        self.memory_usage -= header.size;
        unsafe { std::alloc::dealloc(header_ptr, layout) }

        self.allocations.remove(&(ptr as _));
    }

    pub fn dealloc_vec(&mut self, ptr: *mut u8, layout: Layout) {
        self.memory_usage -= layout.size();
        unsafe { std::alloc::dealloc(ptr, layout) }
    }

    pub fn coalesce(&mut self) {
        // unsafe { self.free_list.coalesce() }
    }

    pub fn debug_print_free_list(&self) {
        // let mut curr = self.free_list.elements;
        // unsafe {
        //     loop {
        //         println!("{:p} -> {:p}| size: {}", curr, (*curr).end_ptr(), (*curr).size);

        //         if let Some(next) = (*curr).next {
        //             curr = next;
        //         }
        //         else { break; }
        //     }

        // }
    }

    pub fn clean_up(&mut self) {
        let allocations = self.allocations.iter().map(|x| *x).collect_vec();
        for alloc in allocations.into_iter() {
            self.dealloc(alloc as *mut u8);
        }
    }
}

// impl Drop for Memory {
//     fn drop(&mut self) {
//         // println!("Dropping Arena: {}", self.size);
//         // unsafe { std::alloc::dealloc(self.buffer, self.layout) }
//     }
// }

#[derive(Debug, Clone)]
pub struct Allocator {
    memory: Arc<Mutex<Memory>>,
}

impl Allocator {
    pub fn new(memory: Arc<Mutex<Memory>>) -> Self {
        Self { memory }
    }
}

unsafe impl std::alloc::Allocator for Allocator {
    fn allocate(&self, layout: Layout) -> Result<std::ptr::NonNull<[u8]>, std::alloc::AllocError> {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .alloc_inner(layout)
    }

    unsafe fn deallocate(&self, ptr: std::ptr::NonNull<u8>, _l: Layout) {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .dealloc(ptr.as_ptr() as *mut _)
    }
}

#[derive(Debug, Clone)]
pub struct VecAllocator {
    memory: Arc<Mutex<Memory>>,
}

impl VecAllocator {
    pub fn new(memory: Arc<Mutex<Memory>>) -> Self {
        Self { memory }
    }
}

unsafe impl std::alloc::Allocator for VecAllocator {
    fn allocate(&self, layout: Layout) -> Result<std::ptr::NonNull<[u8]>, std::alloc::AllocError> {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .alloc_inner_vec(layout)
    }

    unsafe fn deallocate(&self, ptr: std::ptr::NonNull<u8>, l: Layout) {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .dealloc_vec(ptr.as_ptr(), l)
    }
}
