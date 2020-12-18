extern crate debug_cell;
extern crate itertools;
extern crate ordered_float;
pub extern crate oxide;

use std::path::Path;
use std::rc::Rc;

use crate::error::Error;

use crate::ir::hir::HirFile;
use crate::syntax::ParsedFile;
use crate::system::File;

pub mod analysis;
pub mod error;
pub mod generator;
pub mod ir;
pub mod syntax;
pub mod system;
pub mod types;
pub mod utils;

pub trait Executor {
    fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Rc<File>, std::io::Error>;

    fn parse_file(&self, file: &File) -> Result<ParsedFile, Error>;

    fn resolve_root(&mut self, file: ParsedFile) -> Result<HirFile, Error>;

    // fn generate_code(&mut self, file: MirFile) -> Result<(), Error>;
}
