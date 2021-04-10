mod function;
mod instance;
mod module;
mod section;
mod string;
mod structure;
mod tuple;
mod vec;

pub use function::OxFunction;
pub use instance::OxInstance;
pub use module::OxModule;
pub use section::Section;
pub use string::OxString;
pub use structure::OxStruct;
pub use tuple::OxTuple;
pub use vec::OxVec;

pub trait AttributeAccess {
    type Output;

    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output;

    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output;
}
