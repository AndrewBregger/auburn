#![feature(allocator_api)]
extern crate debug_cell;
extern crate itertools;
extern crate ordered_float;
pub extern crate oxide;

use std::{fmt::{self, Display, Formatter}, path::Path, str::FromStr};
use std::rc::Rc;

use crate::error::Error;

use crate::ir::hir::HirFile;
use crate::syntax::ParsedFile;
use crate::system::File;

pub mod analysis;
pub mod code_gen;
pub mod error;
pub mod ir;
pub mod syntax;
pub mod system;
pub mod types;
pub mod utils;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageMode {
    Default,
    Script
}

impl Display for LanguageMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default => write!(f, "default"),
            Self::Script => write!(f, "script"),
        }
    }
}

impl Default for LanguageMode {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Clone)]
pub struct LanguageModeParseError(String);

impl Display for LanguageModeParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for LanguageMode {
    type Err = LanguageModeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "default" => Ok(Self::Default),
            "script" => Ok(Self::Script),
            e => Err(LanguageModeParseError(format!("'{}' is not a valid langauge mode", e))),
        }
    }
}

pub trait Executor {
    fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Rc<File>, std::io::Error>;

    fn parse_file(&self, file: &File) -> Result<ParsedFile, Error>;

    fn resolve_root(&mut self, file: ParsedFile, mode: LanguageMode) -> Result<HirFile, Error>;

    // fn generate_code(&mut self, file: MirFile) -> Result<(), Error>;
}
