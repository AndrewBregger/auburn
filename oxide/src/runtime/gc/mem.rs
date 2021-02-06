use std::alloc::{AllocError, Layout};
use std::{
    ptr::NonNull,
    sync::{Arc, Mutex},
};
// pub static ALIGN_HEADER: usize = std::mem::align_of::<Header>();
// pub static ALIGN_LIST_NODE: usize = std::mem::align_of::<FreeListNode>();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Header {
    // pub id: usize,
    pub size: usize,
}

#[repr(C)]
#[derive(Debug)]
struct FreeListNode {
    size: usize,
    next: Option<&'static mut FreeListNode>,
}

impl FreeListNode {
    fn end_ptr(&self) -> *const u8 {
        unsafe {
            (self as *const FreeListNode as *const u8)
                .add(self.size + std::mem::size_of::<FreeListNode>())
        }
    }

    fn next_to(&self, right: &FreeListNode) -> bool {
        let right_ptr = right as *const FreeListNode as *const u8;
        self.end_ptr() == right_ptr
    }
}

#[derive(Debug, Clone)]
struct FreeList {
    elements: *mut FreeListNode,
}

impl FreeList {
    fn empty(buffer: *mut u8, size: usize) -> Self {
        let node = unsafe {
            let node = std::mem::transmute::<*mut u8, *mut FreeListNode>(buffer);
            (*node).size = size;
            (*node).next = None;
            node
        };

        Self { elements: node }
    }

    unsafe fn insert(&mut self, node: *mut FreeListNode) {}

    unsafe fn coalesce(&mut self) {
        todo!()
    }

    unsafe fn search(&mut self, size: usize) -> (*mut FreeListNode, *mut FreeListNode) {
        self.first_fit_search(size)
    }

    #[inline(always)]
    unsafe fn first_fit_search(&mut self, size: usize) -> (*mut FreeListNode, *mut FreeListNode) {
        todo!()
    }
}

#[derive(Debug)]
pub struct Memory {
    // buffer: *mut u8,
    // end: *const u8,
    layout: Layout,
    size: usize,
    // free_list: FreeList,
}

impl Memory {
    pub fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, 1)
            .expect(format!("failed to allocate Arena({})", size).as_str());

        // let buffer = unsafe { std::alloc::alloc(layout) };
        // let free_list = FreeList::empty(buffer, size);

        Self {
            // buffer,
            // end: unsafe { buffer.add(size) },
            layout,
            size,
            // free_list,
        }
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
            .map(|ptr| ptr.as_ptr() as *mut u8)
    }

    pub fn alloc_inner(&mut self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        println!("allocating: {:?}", layout);
        if layout.size() == 0 {
            return Ok(NonNull::slice_from_raw_parts(NonNull::<u8>::dangling(), 0));
        }

        let layout = Layout::from_size_align(
            layout.size() + std::mem::size_of::<Header>(),
            layout.align(),
        )
        .expect("failed to layout header");

        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            Err(std::alloc::AllocError)
        } else {
            unsafe {
                let header = &mut *(ptr as *mut Header);
                header.size = layout.size();
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
            unsafe {
                let ptr = NonNull::new(ptr).ok_or(std::alloc::AllocError)?;
                Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
            }
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
        unsafe { std::alloc::dealloc(header_ptr, layout) }
    }

    pub fn dealloc_vec(&mut self, ptr: *mut u8, layout: Layout) {
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
