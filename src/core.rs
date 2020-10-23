extern crate clap;

use clap::Clap;
use language::analysis::Analysis;
use language::error::Error;
use language::mir::MirFile;
use language::syntax::token::Operator::Comma;
use language::syntax::{ParsedFile, Parser, Position};
use language::system::{File, FileMap};
use language::utils::{EntityPrinter, MirPrinter};
use language::Executor;
use std::fmt::{Display, Formatter};
use std::io::ErrorKind;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

#[derive(Clap, Debug)]
pub enum Command {
    #[clap()]
    Check { input: String },

    #[clap()]
    Run { input: String },
}

#[derive(Clap, Debug)]
#[clap(version = "0.1.0", author = "Andrew Bregger")]
pub struct Arguments {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug)]
pub enum CoreError {
    IoError(std::io::Error, String),
    // CommandError(CommandError),
    CompilerError(Error),
}

impl From<Error> for CoreError {
    fn from(err: Error) -> Self {
        Self::CompilerError(err)
    }
}

pub struct Core {
    file_map: FileMap,
    analysis: Analysis,
}

impl Core {
    pub fn new() -> Self {
        Self {
            file_map: FileMap::new(),
            analysis: Analysis::new(),
        }
    }

    pub fn run(&mut self) {
        let cmd = Arguments::parse();
        match self.execute(cmd) {
            Ok(()) => {}
            Err(e) => {
                self.print_error(&e);
            }
        }
    }

    fn print_error(&self, err: &CoreError) {
        match err {
            CoreError::IoError(err, file_name) => self.print_io_error(err, file_name),
            CoreError::CompilerError(err) => self.print_compiler_error(err),
        }
    }

    fn print_io_error(&self, err: &std::io::Error, file_name: &str) {
        println!("{}: {}", file_name, err)
    }

    fn print_compiler_error(&self, err: &Error) {
        let pos = err.pos();

        if let Some(file) = self.file_map.file_by_id(&pos.file_id()) {
            let start_coord = pos.start();
            let end_coord = pos.end();
            let line = *file.get_lines(start_coord.0, end_coord.0).first().unwrap();
            println!(
                "{}:{}:{}| {}",
                file.path().display(),
                start_coord.line(),
                start_coord.column(),
                err
            );
            self.print_file_lines(line, pos);
        } else {
            println!("Unable to find file of id: '{}'", pos.file_id().0);
        }
    }

    fn print_file_lines(&self, line: &str, pos: &Position) {
        println!(">\t{}", line);
        Self::print_start_cursor(line, pos.start().line());
    }

    fn print_start_cursor(line: &str, column: usize) {
        let cursor = (0..(column - 1))
            .map(|idx| match line.chars().nth(idx) {
                Some('\t') => '\t',
                Some(_) => ' ',
                None => panic!(),
            })
            .collect::<String>();
        println!(" \t{}^", cursor);
    }

    fn execute(&mut self, command: Arguments) -> Result<(), CoreError> {
        match command.command {
            Command::Check { input } => {
                let file = self
                    .open_file(input.as_str())
                    .map_err(|err| CoreError::IoError(err, input))?;
                let parsed_file = self
                    .parse_file(file.as_ref())
                    .map_err(|err| CoreError::from(err))?;
                let mir_file = self
                    .resolve_root(parsed_file)
                    .map_err(|err| CoreError::from(err))?;

                println!("Global Expressions");
                for stmt in mir_file.expressions() {
                    MirPrinter::print_expr(stmt.as_ref());
                }

                println!("Entities {}", mir_file.entities().len());
                for entity in mir_file.entities() {
                    EntityPrinter::print(&entity.deref().borrow());
                }
            }
            Command::Run { .. } => {}
        }

        Ok(())
    }
}

impl Executor for Core {
    fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Rc<File>, std::io::Error> {
        self.file_map.open_file(path)
    }

    fn parse_file(&self, file: &File) -> Result<ParsedFile, Error> {
        let mut parser = Parser::new(file);
        parser.init()?;
        parser.parse_file()
    }

    fn resolve_root(&mut self, file: ParsedFile) -> Result<MirFile, Error> {
        // got though imports and resolve them
        self.analysis.check(file)
    }
}
