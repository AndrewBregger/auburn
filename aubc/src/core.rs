extern crate clap;

use std::path::Path;
use std::rc::Rc;

use auburn::Executor;
use auburn::{
    analysis::Analysis,
    generator::{CodeGen, Context},
};
use auburn::{error::Error, oxide::vm::Vm};
use auburn::{
    generator::GenError,
    syntax::{ParsedFile, Parser, Position},
};
use auburn::{ir::hir::HirFile, oxide::OxFunction};
use auburn::{
    system::{File, FileMap},
    utils::MirPrinter,
};
use clap::Clap;

#[derive(Clap, Debug)]
enum Command {
    #[clap()]
    Check { input: String },

    #[clap()]
    Parse { input: String },

    #[clap()]
    Run { input: String },

    #[clap()]
    Build { input: String },
}

#[derive(Clap, Debug)]
#[clap(version = "0.1.0", author = "Andrew Bregger")]
struct Arguments {
    #[clap(subcommand)]
    command: Option<Command>,
}

#[derive(Debug)]
enum CoreError {
    IoError(std::io::Error, String),
    // CommandError(CommandError),
    CompilerError(Error),
    BuildError(GenError),
}

impl From<Error> for CoreError {
    fn from(err: Error) -> Self {
        Self::CompilerError(err)
    }
}

impl From<GenError> for CoreError {
    fn from(err: GenError) -> Self {
        Self::BuildError(err)
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
            CoreError::BuildError(err) => self.print_code_gen_error(err),
        }
    }

    fn print_io_error(&self, err: &std::io::Error, file_name: &str) {
        println!("{}: {}", file_name, err)
    }

    fn print_compiler_error(&self, err: &Error) {
        let pos = err.pos();
        println!("Error Pos: {}", pos);
        if let Some(file) = self.file_map.file_by_id(&pos.file_id()) {
            let start_coord = pos.start();
            let _end_coord = pos.end();

            // if start_coord.line() == end_coord.line() {
            let line = file.get_line(start_coord.line());
            println!(
                "{}:{}:{}| {}",
                file.path().display(),
                start_coord.line(),
                start_coord.column(),
                err
            );
            self.print_file_lines(line, pos);
        // } else {
        //     // let line = *file.get_lines(start_coord.0, end_coord.0).first().unwrap();
        //     // println!("Mutli-line error are not supported");
        //
        // }
        } else {
            println!("Unable to find file of id: '{}'", pos.file_id().0);
        }
    }

    fn print_file_lines(&self, line: &str, pos: &Position) {
        println!(">\t{}", line);
        Self::print_line_cursor(line, pos.start().column(), pos.end().column());
    }

    fn print_line_cursor(line: &str, start_column: usize, end_column: usize) {
        let offset = (0..(start_column - 1))
            .map(|idx| match line.chars().nth(idx) {
                Some('\t') => '\t',
                Some(_) => ' ',
                None => panic!("{}|{},{}", line, idx, start_column),
            })
            .collect::<String>();

        if start_column <= end_column {
            let cursor = String::from_utf8(vec![b'^'; end_column - start_column])
                .expect("cursor string is not valid utf8??");
            println!(" \t{}{}", offset, cursor);
        }
    }

    fn print_code_gen_error(&self, err: &GenError) {
        println!("{}", err);
    }

    fn execute(&mut self, arg: Arguments) -> Result<(), CoreError> {
        match arg.command {
            Some(cmd) => self.execute_command(cmd),
            None => self.execute_repl(),
        }
    }

    fn execute_command(&mut self, cmd: Command) -> Result<(), CoreError> {
        match cmd {
            Command::Parse { input } => {
                let file = self
                    .open_file(input.as_str())
                    .map_err(|err| CoreError::IoError(err, input))?;
                let parsed_file = self
                    .parse_file(file.as_ref())
                    .map_err(|err| CoreError::from(err))?;
                println!("{:#?}", parsed_file);
            }
            Command::Check { input } => {
                let file = self
                    .open_file(input.as_str())
                    .map_err(|err| CoreError::IoError(err, input))?;
                let parsed_file = self
                    .parse_file(file.as_ref())
                    .map_err(|err| CoreError::from(err))?;
                let resolved_file = self
                    .resolve_root(parsed_file)
                    .map_err(|err| CoreError::from(err))?;

                MirPrinter::print_file(&resolved_file);
            }
            Command::Run { input } => {
                let file = self
                    .open_file(input.as_str())
                    .map_err(|err| CoreError::IoError(err, input))?;

                let ox_function = self.build(file)?;
                ox_function.disassemble();

                let mut vm = Vm::new();
                match vm.run(ox_function) {
                    Ok(_) => {
                        vm.print_stack();
                    }
                    Err(err) => println!("{}", err),
                }
            }

            Command::Build { input } => {
                let file = self
                    .open_file(input.as_str())
                    .map_err(|err| CoreError::IoError(err, input))?;
                let function = self.build(file)?;
                function.disassemble();
            }
        }
        Ok(())
    }

    fn build(&mut self, file: Rc<File>) -> Result<Box<OxFunction>, CoreError> {
        let parsed_file = self
            .parse_file(file.as_ref())
            .map_err(Into::<CoreError>::into)?;
        let mir_file = self
            .resolve_root(parsed_file)
            .map_err(Into::<CoreError>::into)?;

        let context = Context::new(&self.file_map, &mir_file);
        CodeGen::new(context)
            .build()
            .map_err(|e| CoreError::from(e))
    }

    fn execute_repl(&mut self) -> Result<(), CoreError> {
        println!("REPL not implemented");
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

    fn resolve_root(&mut self, file: ParsedFile) -> Result<HirFile, Error> {
        // got though imports and resolve them
        self.analysis.check(file)
    }
}
