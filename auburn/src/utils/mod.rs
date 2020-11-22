use std::cell::RefCell;
use std::rc::Rc;

pub use entity_printer::EntityPrinter;
pub use mir_printer::MirPrinter;

mod entity_printer;
mod mir_printer;

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn new_ptr<T>(val: T) -> Ptr<T> {
    Rc::new(RefCell::new(val))
}
