use std::{alloc::Layout, ops::Drop};
pub static ALIGN_HEADER: usize = std::mem::align_of::<Header>();
pub static ALIGN_LIST_NODE: usize = std::mem::align_of::<FreeListNode>();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Header {
    // pub id: usize,
    pub size: usize,
    pub next: *const u8,
}

#[repr(C)]
#[derive(Debug)]
struct FreeListNode {
    size: usize,
    next: Option<&'static mut FreeListNode>
}

impl FreeListNode {
    fn end_ptr(&self) -> *const u8 {
        unsafe { (self as *const FreeListNode as *const u8).add(self.size + std::mem::size_of::<FreeListNode>()) }
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

        Self {
            elements: node
        }
    }

    unsafe fn insert(&mut self, node: *mut FreeListNode) {
    }

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
pub(crate) struct Memory {
    buffer: *mut u8,
    end: *const u8,
    layout: Layout,
    size: usize,
    free_list: FreeList,
}

impl Memory {
    pub fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, 1)
            .expect(format!("failed to allocate Arena({})", size).as_str());

        let buffer = unsafe { std::alloc::alloc(layout) };
        let free_list = FreeList::empty(buffer, size);

        Self {
            buffer,
            end: unsafe { buffer.add(size) },
            layout,
            size,
            free_list,
        }
    }

    pub fn contains(&self, ptr: *const u8) -> bool {
        self.buffer as *const _ <= ptr && ptr < self.end as *const _
    }

    pub unsafe fn get_header(&self, ptr: *const u8) -> &Header {
        let header = ptr.sub(std::mem::size_of::<Header>()) as *const Header;
        header.as_ref().expect(format!("should be able to dereference this pointer: {:p}", header).as_str())
    }

    pub fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        let layout = match Layout::from_size_align(size + std::mem::size_of::<Header>(), std::mem::align_of::<Header>()) {
            Ok(layout ) => layout,
            Err(e) => {
                eprintln!("alloc: err {}", e);
                return None
            }
        };

        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            None
        }
        else {
            unsafe {
                let header = &mut *(ptr as *mut Header);
                header.size = size;
                Some(ptr.add(std::mem::size_of::<Header>()))
            }
        }
    }

    pub fn dealloc(&mut self, ptr: *const u8) {
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

    pub fn coalesce(&mut self) {
        unsafe { self.free_list.coalesce() }
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

impl Drop for Memory {
    fn drop(&mut self) {
        // println!("Dropping Arena: {}", self.size);
        unsafe { std::alloc::dealloc(self.buffer, self.layout) }
    }
}
