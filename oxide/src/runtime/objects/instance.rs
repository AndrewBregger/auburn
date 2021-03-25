use std::fmt::Display;

use crate::{
    gc::{Gc, Object, ObjectKind},
    AttributeAccess, OxString, OxStruct, Value,
};

#[derive(Debug, Clone)]
pub struct OxInstance {
    object: Gc<OxStruct>,
    fields: u8,
}

impl OxInstance {
    pub fn new(object: Gc<OxStruct>, fields: u8) -> Self {
        Self { object, fields }
    }

    pub fn name(&self) -> &OxString {
        self.object.name()
    }

    pub fn fields(&self) -> &[Value] {
        let fields = self.fields;
        let ptr = self as *const Self as *const u8;
        let ptr = unsafe { ptr.add(std::mem::size_of::<Self>()) };
        unsafe { std::slice::from_raw_parts(ptr as *const Value, fields as usize) }
    }

    pub fn fields_mut(&mut self) -> &mut [Value] {
        let fields = self.fields;
        let ptr = self as *mut Self as *mut u8;
        let ptr = unsafe { ptr.add(std::mem::size_of::<Self>()) };
        unsafe { std::slice::from_raw_parts_mut(ptr as *mut Value, fields as usize) }
    }

    pub fn disassemble(&self) {
        println!("{}", self);
    }
}

impl Display for OxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {}>", self.name())
    }
}

impl Object for OxInstance {
    fn object_kind() -> ObjectKind {
        ObjectKind::String
    }
}

impl AttributeAccess for OxInstance {
    type Output = Value;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output {
        &self.fields()[idx]
    }

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output {
        &mut self.fields_mut()[idx]
    }
}
