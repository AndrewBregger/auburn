use crate::gc::{Cell, GcObject};

#[derive(Debug, Clone, Copy)]
pub struct OxStruct {
    cell: Cell,
}

impl OxStruct {}

impl GcObject for OxStruct {
    fn as_cell(&self) -> &Cell {
        &self.cell
    }

    fn as_cell_mut(&mut self) -> &mut Cell {
        &mut self.cell
    }
}
