extern crate language;

use std::error::Error;

use language::file::File;
use language::syntax::TokenCursor;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("examples/lexer/tree_tokens")?;
    // let file = File::open("examples/lexer/linear_tokens")?;
    for token in TokenCursor::new(file.content(), file.id()) {
        match token {
            Ok(token) => println!("{}", token),
            Err(e) => println!("{:?}", e),
        }
    }
    Ok(())
}
