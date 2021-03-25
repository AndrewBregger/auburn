use super::ObjectKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
