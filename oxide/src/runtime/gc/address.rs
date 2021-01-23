use std::ops::{Add, Sub};

use crate::runtime::gc::GcObject;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address(usize);

impl Address {
    pub fn from_ptr(ptr: *const u8) -> Self {
        Self(ptr as _)
    }
    pub fn null() -> Self {
        Self(0)
    }
    pub fn into_ref<T: GcObject>(&self) -> &T {
        unsafe { &*(self.0 as *const u8 as *const T) }
    }

    pub fn into_ref_mut<T: GcObject>(&mut self) -> &mut T {
        unsafe { &mut *(self.0 as *mut u8 as *mut T) }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0 as *const u8
    }

    pub fn as_ptr_mut(&self) -> *mut u8 {
        self.0 as *mut u8
    }
}

impl Add<usize> for Address {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl Sub<usize> for Address {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}
