#[derive(Debug, Clone, Copy, Default)]
pub struct Cell {
    marked: bool,
}

impl Cell {
    pub fn mark(&mut self, val: bool) {
        self.marked = val;
    }
}

pub trait GcObject {
    fn as_cell(&self) -> &Cell;
    fn as_cell_mut(&mut self) -> &mut Cell;
}
