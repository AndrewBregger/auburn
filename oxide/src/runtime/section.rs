use std::fmt::{Display, Formatter};
use std::slice::SliceIndex;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::vm::OpCode;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct SectionId(usize);

impl SectionId {
    pub fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(1);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Display for SectionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// section of executable byte code.
#[derive(Debug, Clone, Default)]
pub struct Section {
    // I am wondering if it would denifit from storing a reference to a array of
    // bytes instead of owning it. This would mean that the section is not able
    // to write to its data since it is owned outside of it.
    data: Vec<u8>,
    id: SectionId,
}

impl Section {
    pub fn new() -> Self {
        Self {
            data: vec![],
            id: SectionId::next(),
        }
    }

    pub fn id(&self) -> SectionId {
        self.id
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.data.push(byte);
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.data.push(op as u8);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes)
    }

    pub fn read<I>(&self, index: I) -> Option<&<I as SliceIndex<[u8]>>::Output>
    where
        I: SliceIndex<[u8]>,
    {
        self.data.get(index)
    }

    pub unsafe fn read_unchecked<I>(&self, index: I) -> &<I as SliceIndex<[u8]>>::Output
    where
        I: SliceIndex<[u8]>,
    {
        self.data.get_unchecked(index)
    }

    pub fn debug_print(&self) {
        for value in &self.data {
            println!("{:#x}", value);
        }
    }
}
