mod entity_printer;
mod mir_printer;

pub use entity_printer::EntityPrinter;
pub use mir_printer::MirPrinter;
use std::any::type_name;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn new_ptr<T>(val: T) -> Ptr<T> {
    Rc::new(RefCell::new(val))
}
