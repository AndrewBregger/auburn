extern crate language;

use std::error::Error;

use language::analysis::Analysis;
use language::syntax::Parser;
use language::system::{File, FileMap};
use language::utils::{EntityPrinter, MirPrinter};
use std::ops::Deref;

fn compile(root: &File) -> Result<(), language::error::Error> {
    let mut parser = Parser::new(root);
    let mut analysis = Analysis::new();

    parser.init()?;

    let parsed_file = parser.parse_file()?;
    let checked = analysis.check(parsed_file)?;

    for stmt in checked.stmts() {
        MirPrinter::print_stmt(stmt.as_ref());
    }

    for entity in checked.entities() {
        EntityPrinter::print(&entity.deref().borrow());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut file_map = FileMap::new();
    let file = file_map.open_file("examples/analysis/variable")?;

    match compile(file) {
        Ok(_) => {}
        Err(e) => println!("{}", e),
    }
    Ok(())
}
