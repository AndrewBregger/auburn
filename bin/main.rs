extern crate language;

use std::error::Error;

use language::analysis::{Analysis, Entity};
use language::mir::{MirNode, MirNodeBase, MirStmt};
use language::syntax::ast::NodeType;
use language::syntax::{ParsedFile, Parser};
use language::system::FileMap;
use std::borrow::Borrow;
use std::ops::Deref;

fn print_stmt(stmt: Box<MirStmt>) {
    print_node_impl(stmt.as_ref(), 0)
}

fn indent(num: usize) -> String {
    (0..num).map(|_| '\n').collect()
}

fn print_node_impl<N: NodeType>(node: &MirNodeBase<N>, index: usize) {
    println!("{}{}\t{}", indent(index), node.inner().name(), node.ty())
}

fn print_entity(entity: &Entity) {}

fn main() -> Result<(), Box<dyn Error>> {
    let mut file_map = FileMap::new();
    let file = file_map.open_file("examples/analysis/expr")?;
    let mut analysis = Analysis::new();
    let mut parser = Parser::new(file);
    parser.init().unwrap();
    match parser.parse_file() {
        Ok(file) => match analysis.check(file) {
            Ok(checked) => {
                for stmt in checked.stmts() {
                    print_stmt(stmt.clone());
                }

                for entity in checked.entities() {
                    print_entity(&entity.deref().borrow());
                }
            }
            Err(err) => println!("{}", err),
        },
        Err(err) => println!("{}", err),
    }
    Ok(())
}
