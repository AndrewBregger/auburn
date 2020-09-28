extern crate language;

use std::error::Error;

use language::file::File;
use language::syntax::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("examples/parser/item/variables")?;
    let mut parser = Parser::new(&file);
    parser.consume().expect("Initializing the parser");
    match parser.parse_stmt() {
        Ok(expr) => println!("{:#?}", expr),
        Err(e) => println!("{}", e),
    }
    Ok(())
}
