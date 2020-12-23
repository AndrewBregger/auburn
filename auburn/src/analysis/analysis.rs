use crate::analysis::entity::EntityInfo;
use crate::analysis::entity::Path;
use crate::analysis::scope::{Scope, ScopeKind};
use crate::analysis::typer::Typer;
use crate::analysis::Entity;
use crate::error::Error;
use crate::ir::ast::Visibility;
use crate::ir::hir::HirFile;
use crate::syntax::ParsedFile;
use crate::types::TypeMap;

pub struct Analysis {
    type_map: TypeMap,
    scope_stack: Vec<Scope>,
}

impl Analysis {
    pub fn new() -> Self {
        let mut analysis = Analysis {
            type_map: TypeMap::new(),
            scope_stack: vec![],
        };

        analysis.type_map.init_primitives();
        analysis.load_prelude();

        analysis
    }

    fn load_prelude(&mut self) {
        macro_rules! define_primitive {
            ($scope:expr, $name:literal, $ty:expr) => {
                $scope.add_element(
                    $name,
                    Entity::new_ref(
                        Visibility::Public,
                        $name.to_string(),
                        $ty,
                        EntityInfo::Primitive,
                        Path::empty(),
                    ),
                );
            };
        }
        let prelude_scope = Scope::new(ScopeKind::Prelude, None);
        self.scope_stack.push(prelude_scope);

        let prelude_scope = self.scope_stack.last_mut().unwrap();

        define_primitive!(prelude_scope, "u8", self.type_map.get_u8());
        define_primitive!(prelude_scope, "u16", self.type_map.get_u16());
        define_primitive!(prelude_scope, "u32", self.type_map.get_u32());
        define_primitive!(prelude_scope, "u64", self.type_map.get_u64());

        define_primitive!(prelude_scope, "i8", self.type_map.get_i8());
        define_primitive!(prelude_scope, "i16", self.type_map.get_i16());
        define_primitive!(prelude_scope, "i32", self.type_map.get_i32());
        define_primitive!(prelude_scope, "i64", self.type_map.get_i64());

        define_primitive!(prelude_scope, "f32", self.type_map.get_f32());
        define_primitive!(prelude_scope, "f64", self.type_map.get_f64());

        define_primitive!(prelude_scope, "char", self.type_map.get_char());
        define_primitive!(prelude_scope, "bool", self.type_map.get_bool());

        // load prelude
    }

    pub fn check(&mut self, file: ParsedFile) -> Result<HirFile, Error> {
        Typer::new(&mut self.type_map, &mut self.scope_stack).resolve_file(file)
    }
}
