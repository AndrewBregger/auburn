#[derive(Debug, Clone, Copy)]
pub enum ObjectKind {
    Buffer,
    Function,
    Struct,
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

pub trait GcObject: Send + Sync + Clone + Copy {
    fn as_cell(&self) -> &Cell;
    fn as_cell_mut(&mut self) -> &mut Cell;
}
