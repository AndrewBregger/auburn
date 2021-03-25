use std::{
    ops::{Deref, DerefMut},
    vec::Vec as VecInnerImpl,
};
type VecInner<Ty> = VecInnerImpl<Ty, VecAllocator>;

use crate::gc::{Object, ObjectKind, VecAllocator};

#[derive(Debug, Clone)]
pub struct OxVec<Ty> {
    inner: VecInner<Ty>,
}

impl<Ty> OxVec<Ty> {
    pub fn new(allocator: VecAllocator) -> Self {
        Self {
            inner: VecInner::new_in(allocator),
        }
    }

    pub fn with_capacity(allocator: VecAllocator, len: usize) -> Self {
        Self {
            inner: VecInner::with_capacity_in(len, allocator),
        }
    }
}

impl<Ty> Deref for OxVec<Ty> {
    type Target = VecInner<Ty>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<Ty> DerefMut for OxVec<Ty> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<Ty> Object for OxVec<Ty> {
    fn object_kind() -> ObjectKind {
        ObjectKind::Vec
    }
}
