extern crate language;

use std::error::Error;

use language::file::File;
use language::syntax::{Parser, TokenCursor};

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("examples/parser/expr/block")?;
    let mut parser = Parser::new(&file);
    parser.consume().expect("Initializing the parser");
    match parser.parse_expr() {
        Ok(expr) => println!("{:#?}", expr),
        Err(e) => println!("{:?}", e),
    }
    Ok(())
}
