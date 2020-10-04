use crate::analysis::scope::ScopeRef;
use crate::analysis::typer::Typer;
use crate::analysis::Entity;
use crate::error::Error;
use crate::mir::MirFile;
use crate::syntax::ast::Item;
use crate::syntax::ParsedFile;
use crate::types::TypeMap;

pub struct Analysis {
    type_map: TypeMap,
    item_stack: Vec<Box<Item>>,
    scope_stack: Vec<ScopeRef>,
    entity_stack: Vec<Entity>,
}

impl Analysis {
    pub fn new() -> Self {
        let mut analysis = Analysis {
            type_map: TypeMap::new(),
            item_stack: vec![],
            scope_stack: vec![],
            entity_stack: vec![],
        };

        analysis.type_map.init_primitives();

        analysis
    }

    pub fn check(&mut self, file: ParsedFile) -> Result<MirFile, Error> {
        let typed_file = Typer::new(
            &mut self.type_map,
            &mut self.item_stack,
            &mut self.scope_stack,
        )
        .resolve_file(file)?;

        Ok(typed_file)
    }
}
