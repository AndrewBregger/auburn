use std::ops::{Add, Sub};

use crate::{OxFunction, OxInstance, OxModule, OxString, OxStruct, OxTuple, Value};

use super::{Cell, Gc, Header, Object, ObjectKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address(usize);

impl Address {
    pub fn from_ptr(ptr: *const u8) -> Self {
        Self(ptr as _)
    }
    pub fn null() -> Self {
        Self(0)
    }
    pub fn into_ref<T: Object>(&self) -> &T {
        unsafe { &*(self.0 as *const u8 as *const T) }
    }

    pub fn into_ref_mut<T: Object>(&mut self) -> &mut T {
        unsafe { &mut *(self.0 as *mut u8 as *mut T) }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0 as *const u8
    }

    pub fn as_ptr_mut(&self) -> *mut u8 {
        self.0 as *mut u8
    }

    pub fn cell(&self) -> &Cell {
        let header =
            unsafe { &*(self.as_ptr().sub(std::mem::size_of::<Header>()) as *const Header) };
        header.cell()
    }

    pub fn cell_mut(&mut self) -> &mut Cell {
        let header =
            unsafe { &mut *(self.as_ptr_mut().sub(std::mem::size_of::<Header>()) as *mut Header) };
        header.cell_mut()
    }

    pub fn mark(&mut self, val: bool) {
        self.cell_mut().mark(val)
    }
}

impl Into<Value> for Address {
    fn into(self) -> Value {
        let cell = self.cell();
        match cell.kind {
            ObjectKind::Instance => Value::from(Gc::<OxInstance>::new(self)),
            ObjectKind::Module => Value::from(Gc::<OxModule>::new(self)),
            ObjectKind::String => Value::from(Gc::<OxString>::new(self)),
            ObjectKind::Struct => Value::from(Gc::<OxStruct>::new(self)),
            ObjectKind::Tuple => Value::from(Gc::<OxTuple>::new(self)),
            ObjectKind::Function => Value::from(Gc::<OxFunction>::new(self)),
            _ => {
                panic!(
                    "Attempting to get a value from object of kind: {:?}",
                    cell.kind
                )
            }
        }
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
