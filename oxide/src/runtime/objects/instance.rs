use std::fmt::Display;

use crate::{AttributeAccess, OxString, OxStruct, Value, gc::{Cell, Gc, GcObject, ObjectKind}};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct OxInstance {
    cell: Cell,
    pub object: Gc<OxStruct>,
    pub fields: u16,
}

impl OxInstance {
    pub fn new(object: Gc<OxStruct>, fields: u16) -> Self {
        Self {
            cell: Cell::new(ObjectKind::Instance),
            object,
            fields,
        }
    }

    pub fn len(&self) -> u16 {
        self.fields
    }

    pub fn fields_ptr(&self) -> *const Value {
        unsafe {
            (self as *const OxInstance as *const u8).add(std::mem::size_of::<OxInstance>())
                as *const Value
        }
    }

    pub fn fields_ptr_mut(&mut self) -> *mut Value {
        unsafe {
            (self as *mut OxInstance as *mut u8).add(std::mem::size_of::<OxInstance>())
                as *mut Value
        }
    }
}

impl Display for OxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {}>", self.object.name())
    }
}

impl GcObject for OxInstance {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}

impl AttributeAccess for OxInstance {
    type Output = Value;


    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        let fields_ptr = self.fields_ptr();
        
        unsafe { &*fields_ptr.add(idx) }
    }
    
    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        let fields_ptr_mut = self.fields_ptr_mut();
        unsafe { &mut *fields_ptr_mut.add(idx) }
    }
}
