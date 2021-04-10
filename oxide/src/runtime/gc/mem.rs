use std::{
    alloc::{AllocError, Layout},
    collections::BTreeSet,
    fmt::Display,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
    sync::{Arc, Mutex},
    time::SystemTime,
};

use crate::bit_map::BitMap;

use super::{Cell, ObjectKind};

// pub static ALIGN_HEADER: usize = std::mem::align_of::<Header>();
// pub static ALIGN_LIST_NODE: usize = std::mem::align_of::<FreeListNode>();
static COLLECT_INITIAL: usize = 256; //1024 * 1024;
static COLLECT_FACTOR: f64 = 1.5;
static DEFAULT_POOL_SIZE: usize = 1024;

struct FreeNode {
    size: usize,
    prev: *mut FreeNode,
    next: *mut FreeNode,
}

impl FreeNode {
    pub fn new(size: usize) -> Self {
        Self {
            size,
            prev: std::ptr::null_mut(),
            next: std::ptr::null_mut(),
        }
    }

    unsafe fn is_consecutive(left: *mut Self, right: *mut Self) -> bool {
        let left_end = (left as *mut u8).add((*left).size);
        left_end == right as *mut u8
    }
}

#[derive(Debug, Clone)]
pub struct Pool {
    buffer: *mut u8,
    element_size: usize,
    len: usize,
    bitmap: BitMap,
}

impl Drop for Pool {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(
                self.buffer,
                Layout::from_size_align_unchecked(self.len(), 1),
            );
        }
    }
}

impl Pool {
    pub fn new(element_size: usize, elements: usize) -> Self {
        let buffer = unsafe {
            std::alloc::alloc(Layout::from_size_align_unchecked(
                element_size * elements,
                1,
            ))
        };
        Self {
            buffer,
            element_size,
            len: element_size * elements,
            bitmap: BitMap::new(elements),
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn free(&self) -> usize {
        self.len() - self.allocated()
    }

    pub fn allocated(&self) -> usize {
        self.bitmap.count_ones() * self.element_size
    }

    pub fn alloc(&mut self) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        if let Some(index) = self.bitmap.first_zero() {
            self.bitmap.set(index, true);
            let ptr = self.ptr(index);
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            Ok(NonNull::slice_from_raw_parts(ptr, self.element_size))
        } else {
            Err(std::alloc::AllocError)
        }
    }

    pub fn dealloc(&mut self, ptr: *mut u8) {
        if self.buffer <= ptr && ptr < unsafe { self.buffer.add(self.len) } {
            let diff = ptr as usize - self.buffer as usize;
            let rem = diff % self.element_size;
            assert_eq!(rem, 0);

            let slot = diff / self.element_size;
            self.bitmap.set(slot, false);
        }
    }

    pub fn ptr(&self, idx: usize) -> *mut u8 {
        unsafe { self.buffer.add(idx * self.element_size) }
    }

    pub fn dealloc_unmarked(&mut self) {
        // for each allocated slot check if it has been marked.
        for (idx, byte) in self.bitmap.clone().iter().enumerate() {
            for i in 0..8 {
                if (*byte >> i) & 0x1 == 1 {
                    let index = idx * 8 + i;
                    let ptr = self.ptr(index);
                    let header = unsafe { &mut *(ptr as *mut Header) };
                    // let kind = cell.kind.clone();
                    if header.cell.marked {
                        header.cell.mark(false);
                    } else {
                        println!(
                            "Deallocating: {:p} {:p} {:?}",
                            ptr,
                            unsafe { ptr.add(std::mem::size_of::<Header>()) },
                            header.cell.kind
                        );
                        self.dealloc(ptr);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arena {
    #[allow(dead_code)]
    buffer: *mut u8,
    #[allow(dead_code)]
    len: usize,
    allocated: usize,
    // free list elements
    head: *mut FreeNode,
    tail: *mut FreeNode,
}

impl Drop for Arena {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.buffer, Layout::from_size_align_unchecked(self.len, 1));
        }
    }
}

impl Arena {
    pub fn new(len: usize) -> Self {
        Self {
            buffer: std::ptr::null_mut(),
            len,
            allocated: 0,
            head: std::ptr::null_mut(),
            tail: std::ptr::null_mut(),
        }
    }

    #[allow(dead_code)]
    fn init(&mut self) {
        let layout;
        let buffer;
        let node;

        unsafe {
            layout = Layout::from_size_align_unchecked(self.len, 1);
            buffer = std::alloc::alloc(layout);

            node = buffer as *mut FreeNode;
            *node = FreeNode::new(self.len);
        }

        self.buffer = buffer;
        self.head = node;
        self.tail = node;
    }

    pub fn allocated(&self) -> bool {
        self.buffer != std::ptr::null_mut()
    }

    pub fn alloc(&mut self, size: usize) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        if self.allocated + size < self.len {
            return Err(std::alloc::AllocError);
        }

        unsafe {
            let node = self.find_by_size(size).ok_or(std::alloc::AllocError)?;
            let alloc = node as *mut u8;

            // this becomes a new node of the remaining free
            let rest = alloc.add(size) as *mut FreeNode;
            let remaining_size = (*node).size - size;

            let prev = (*node).prev;
            let next = (*node).next;

            if (*node).size == size {
                // the node was completely used.
                (*prev).next = next;
                (*next).prev = prev;
            } else {
                // recreate the new node
                *rest = FreeNode::new(remaining_size);
                if !prev.is_null() {
                    (*prev).next = rest;
                    (*rest).prev = prev;
                }
                if !next.is_null() {
                    (*rest).next = next;
                    (*next).prev = rest;
                }

                println!("Remaining: {}", remaining_size);
                println!("Node : {:p}", node);
                println!("Node: {:p}", prev);
                println!("Node: {:p}", next);

                if self.head.eq(&node) {
                    self.head = rest;
                }

                if self.tail.eq(&node) {
                    self.tail = rest;
                }
            }

            self.allocated += size;
            let ptr = NonNull::new_unchecked(alloc);
            Ok(NonNull::slice_from_raw_parts(ptr, size))
        }
    }

    pub fn dealloc(&mut self, ptr: *mut u8, size: usize) {
        if let Some(node) = unsafe { self.find_free_node_after(ptr) } {
            let prev = unsafe { (*node).prev };
            let next = unsafe { (*node).next };
            assert!(size >= std::mem::size_of::<FreeNode>());

            let node = ptr as *mut FreeNode;
            unsafe {
                *node = FreeNode::new(size);
                (*node).prev = prev;
                (*node).next = next;

                self.local_collease(prev, node, next);
            }

            self.allocated -= size;
        }
    }

    #[allow(dead_code)]
    fn validate(&self) -> bool {
        let mut curr = self.head;
        let mut allocated_size = 0;
        while !curr.is_null() {
            unsafe {
                allocated_size += (*curr).size;
                curr = (*curr).next;
            }
        }
        allocated_size + self.allocated == self.len
    }

    unsafe fn local_collease(
        &self,
        left: *mut FreeNode,
        middle: *mut FreeNode,
        right: *mut FreeNode,
    ) {
        // combine middle and right if possible
        if FreeNode::is_consecutive(middle, right) {
            let next = (*right).next;
            (*middle).next = next;
            (*middle).size += (*right).size;
        }

        // combine left and middle is possible
        if FreeNode::is_consecutive(left, middle) {
            let next = (*middle).next;
            (*left).next = next;
            (*left).size += (*middle).size;
        }
    }

    unsafe fn find_free_node_after(&self, ptr: *mut u8) -> Option<*mut FreeNode> {
        let mut curr = self.head;
        while !curr.is_null() {
            if ptr.le(&(curr as *mut u8)) {
                curr = (*curr).next;
            } else {
                break;
            }
        }

        let curr_ref = &mut *curr;
        assert!((curr_ref.prev as *mut u8).le(&ptr));
        Some(curr)
    }

    unsafe fn find_by_size(&self, size: usize) -> Option<*mut FreeNode> {
        let mut curr = self.head;

        while !curr.is_null() {
            let curr_ref = &mut *curr;
            if size <= curr_ref.size {
                return Some(curr);
            } else {
                curr = curr_ref.next;
            }
        }

        None
    }

    pub fn print_free_list(&self) {
        let mut curr = self.head;
        let mut index = 0;
        while !curr.is_null() {
            let curr_ref = unsafe { &*curr };
            println!("{}| {}", index, curr_ref.size);
            index += 1;

            curr = unsafe { (*curr).next };
        }
    }

    fn dealloc_unmarked(&mut self) {
        // todo!()
    }
}

#[repr(packed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Header {
    layout: Layout,
    cell: Cell,
}

impl Header {
    pub fn new(object_kind: ObjectKind, layout: Layout) -> Self {
        Self {
            layout,
            cell: Cell::new(object_kind),
        }
    }

    pub fn is_marked(&self) -> bool {
        self.cell.marked
    }

    pub fn mark(&mut self, val: bool) {
        self.cell.mark(val)
    }

    pub fn cell(&self) -> &Cell {
        &self.cell
    }

    pub fn cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}

#[derive(Debug, Clone)]
pub enum AllocationKind {
    Alloc,
    DeAlloc,
}

#[cfg(debug_assertions)]
#[derive(Debug, Clone)]
pub struct AllocationRecord {
    pub id: usize,
    pub kind: AllocationKind,
    pub allocation: NonNull<[u8]>,
    pub layout: Layout,
    pub time: SystemTime,
}

#[cfg(debug_assertions)]
impl AllocationRecord {
    pub fn new(kind: AllocationKind, allocation: NonNull<[u8]>, layout: Layout) -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: TOKEN.fetch_add(1, Ordering::SeqCst),
            kind,
            allocation,
            layout,
            time: SystemTime::now(),
        }
    }
}

#[cfg(debug_assertions)]
impl Display for AllocationRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AllocationRecord[{}]({:?}, layout: {}, {}, time: {:?})",
            self.id,
            self.allocation.as_ptr(),
            self.layout.size(),
            self.layout.align(),
            self.time
        )
    }
}

#[derive(Debug, Clone)]
pub struct Memory {
    next_collect: usize,
    allocations: BTreeSet<usize>,
    pools: Vec<Pool>,
    arena: Arena,
    start_power: usize,

    #[cfg(debug_assertions)]
    allocation_records: Vec<AllocationRecord>,
}

impl Memory {
    pub fn new() -> Self {
        let mut mem = Self {
            next_collect: COLLECT_INITIAL,
            allocations: BTreeSet::new(),
            pools: vec![],
            arena: Arena::new(4048),
            start_power: 0,

            #[cfg(debug_assertions)]
            allocation_records: vec![],
        };

        mem.init_default_pools();
        mem
    }

    #[inline(always)]
    pub fn memory_usage(&self) -> usize {
        self.pools.iter().map(|pool| pool.allocated()).sum()
    }

    #[inline(always)]
    pub fn should_collect(&self) -> bool {
        self.memory_usage() >= self.next_collect
    }

    pub fn update_next_collection(&mut self) {
        self.next_collect = (self.next_collect as f64 * COLLECT_FACTOR) as usize;
    }

    pub unsafe fn get_header(&self, ptr: *const u8) -> &Header {
        let header = ptr.sub(std::mem::size_of::<Header>()) as *const Header;
        header
            .as_ref()
            .expect(format!("should be able to dereference this pointer: {:p}", header).as_str())
    }

    pub fn alloc(&mut self, kind: ObjectKind, layout: Layout) -> Option<*mut u8> {
        self.alloc_inner(kind, layout)
            .ok()
            .map(|ptr| ptr.as_ptr() as *mut u8)
            .and_then(|ptr| {
                self.allocations.insert(ptr as _);
                Some(ptr)
            })
    }

    pub fn alloc_inner(
        &mut self,
        kind: ObjectKind,
        layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let old_size = layout.size();
        let size = (layout.size() + std::mem::size_of::<Header>()).next_power_of_two();
        let layout = unsafe { Layout::from_size_align_unchecked(size, 1) };
        // println!("allocating size: {}, with header: {}", old_size, size);

        let ptr = self.alloc_base(size)?;

        let header = ptr.as_ptr() as *mut u8 as *mut Header;
        println!("Allocating: {:p} {:?}", header, kind);
        unsafe { *header = Header::new(kind, layout) };
        let ptr = unsafe { (header as *mut u8).add(std::mem::size_of::<Header>()) };
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        let ptr = NonNull::slice_from_raw_parts(ptr, old_size);

        if cfg!(debug_assertions) {
            self.push_allocation_record(ptr, layout);
            // println!("{}", self.last_record().unwrap());
        }

        // println!("{}", std::backtrace::Backtrace::force_capture());

        Ok(ptr)
    }

    fn alloc_base(&mut self, size: usize) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        // this is really expensive for each allocation
        let index = (size as f64).log2().floor() as usize - self.start_power;
        if index < self.pools.len() {
            self.pools[index].alloc()
        } else {
            panic!("Arena allocator not implemented yet")
        }
    }

    pub fn dealloc(&mut self, ptr: *mut u8) {
        let header_ptr = unsafe { ptr.sub(std::mem::size_of::<Header>()) } as *mut u8;
        let header = unsafe { &*(header_ptr as *const Header) };
        let layout = header.layout;
        self.dealloc_inner(header_ptr, layout);
    }

    fn dealloc_inner(&mut self, ptr: *mut u8, layout: Layout) {
        if cfg!(debug_assertions) {
            let ptr = NonNull::slice_from_raw_parts(
                NonNull::new(ptr)
                    .expect("deallocating a null ptr, this should have been caught already"),
                layout.size(),
            );

            self.push_deallocation_record(ptr, layout);
        }
        let index = (layout.size() as f64).log2().floor() as usize - self.start_power;
        if index < self.pools.len() {
            self.pools[index].dealloc(ptr);
        } else {
            panic!("Arean allocator not implemented")
        }
    }

    fn init_default_pools(&mut self) {
        let header_size = std::mem::size_of::<Header>();
        let mut size = header_size.next_power_of_two();
        self.start_power = (size as f64).log2().floor() as usize;
        for _ in 0..5 {
            self.pools.push(Pool::new(size, DEFAULT_POOL_SIZE));
            size = (size + 1).next_power_of_two();
        }
        // for pool in self.pools.iter() {
        //     println!("size: {}", pool.element_size);
        // }
    }

    #[cfg(debug_assertions)]
    pub fn push_allocation_record(&mut self, ptr: NonNull<[u8]>, layout: Layout) {
        let record = AllocationRecord::new(AllocationKind::Alloc, ptr, layout);
        self.allocation_records.push(record);
    }

    #[cfg(debug_assertions)]
    pub fn push_deallocation_record(&mut self, ptr: NonNull<[u8]>, layout: Layout) {
        let record = AllocationRecord::new(AllocationKind::DeAlloc, ptr, layout);
        self.allocation_records.push(record);
    }

    #[cfg(debug_assertions)]
    pub fn last_record(&self) -> Option<&AllocationRecord> {
        self.allocation_records.last()
    }

    #[cfg(debug_assertions)]
    pub fn records(&self) -> &[AllocationRecord] {
        self.allocation_records.as_slice()
    }

    pub(crate) fn dump_mem_stats(&self) {
        for pool in self.pools.iter() {
            println!(
                "Pool {}: Allocated: {}, Free: {}",
                pool.element_size,
                pool.allocated(),
                pool.free()
            );
        }

        println!("Arena: not implemented");
    }

    pub fn sweep(&mut self) {
        for pool in self.pools.iter_mut() {
            pool.dealloc_unmarked();
        }
        self.arena.dealloc_unmarked();
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
            .alloc_inner(ObjectKind::Vec, layout)
    }

    unsafe fn deallocate(&self, ptr: std::ptr::NonNull<u8>, _l: Layout) {
        self.memory
            .lock()
            .expect("failed to retrieve memory lock")
            .dealloc(ptr.as_ptr())
    }
}
