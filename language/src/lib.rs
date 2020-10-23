extern crate debug_cell;
extern crate itertools;
extern crate ordered_float;

use crate::error::Error;
use crate::mir::MirFile;
use crate::syntax::ParsedFile;
use crate::system::File;
use std::path::Path;
use std::rc::Rc;

pub mod analysis;
pub mod error;
pub mod mir;
pub mod syntax;
pub mod system;
pub mod types;
pub mod utils;

pub trait Executor {
    fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Rc<File>, std::io::Error>;

    fn parse_file(&self, file: &File) -> Result<ParsedFile, Error>;

    fn resolve_root(&mut self, file: ParsedFile) -> Result<MirFile, Error>;

    // fn generate_code(&mut self, file: MirFile) -> Result<(), Error>;
}
