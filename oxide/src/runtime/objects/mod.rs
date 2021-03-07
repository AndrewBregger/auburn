mod function;
mod instance;
mod module;
mod string;
mod structure;
mod vec_buffer;
mod tuple;

pub use function::OxFunction;
pub use instance::OxInstance;
pub use module::OxModule;
pub use string::OxString;
pub use structure::OxStruct;
pub use tuple::OxTuple;
pub use vec_buffer::VecBuffer;

pub trait AttributeAccess {
    type Output;
    
    fn get_attr(&self, idx: usize) -> &<Self as AttributeAccess>::Output;

    
    fn get_attr_mut(&mut self, idx: usize) -> &mut <Self as AttributeAccess>::Output;


    // maybe there could be by name methods?
}
