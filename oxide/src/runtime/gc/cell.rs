#[derive(Debug, Clone, Copy)]
pub enum ObjectKind {
    /// manually managed buffer,
    Buffer,
    /// vec managed buffer (dynmaic buffer using vecs implementation)
    VecBuffer,
    /// function object
    Function,
    /// structure instance object
    Instance,
    // structure object
    Struct,
    /// module object
    Module,
    // tuple object
    Tuple,
}

#[derive(Debug, Clone, Copy)]
pub struct Cell {
    pub kind: ObjectKind,
    pub marked: bool,
}

impl Cell {
    pub fn new(kind: ObjectKind) -> Self {
        Self {
            kind,
            marked: false,
        }
    }
    pub fn mark(&mut self, val: bool) {
        self.marked = val;
    }
}

pub trait GcObject: Send + Sync + Clone {
    fn as_cell(&self) -> &Cell;
    fn as_cell_mut(&mut self) -> &mut Cell;
}
