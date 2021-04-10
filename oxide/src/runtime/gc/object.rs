#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectKind {
    Section,
    Vec,
    Function,
    Instance,
    Module,
    String,
    Struct,
    Tuple,
}

pub trait Object {
    fn object_kind() -> ObjectKind;
}
