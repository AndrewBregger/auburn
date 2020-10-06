use crate::types::ty::{Type, TypeId, TypeKind};
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct TypeMap {
    map: BTreeMap<TypeId, Rc<Type>>,
}

impl TypeMap {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    pub fn init_primitives(&mut self) {
        self.create_type(Type::new(TypeKind::Invalid));
        self.create_type(Type::new(TypeKind::U8));
        self.create_type(Type::new(TypeKind::U16));
        self.create_type(Type::new(TypeKind::U32));
        self.create_type(Type::new(TypeKind::U64));

        self.create_type(Type::new(TypeKind::I8));
        self.create_type(Type::new(TypeKind::I16));
        self.create_type(Type::new(TypeKind::I32));
        self.create_type(Type::new(TypeKind::I64));

        self.create_type(Type::new(TypeKind::F32));
        self.create_type(Type::new(TypeKind::F64));

        self.create_type(Type::new(TypeKind::Bool));
        self.create_type(Type::new(TypeKind::Char));
        self.create_type(Type::new(TypeKind::Unit));
    }

    pub fn insert_type(&mut self, kind: TypeKind) -> Rc<Type> {
        for ty in self.map.values() {
            if ty.kind() == kind {
                return ty.clone();
            }
        }

        let new_type = Rc::new(Type::new(kind));
        let id = new_type.id();
        self.map.insert(id, new_type.clone());
        new_type
    }

    fn create_type(&mut self, ty: Type) {
        let id = ty.id();
        self.map.insert(id, Rc::new(ty));
    }

    pub fn get_u8(&self) -> Rc<Type> {
        let id = TypeId(1);
        self.map[&id].clone()
    }

    pub fn get_u16(&self) -> Rc<Type> {
        let id = TypeId(2);
        self.map[&id].clone()
    }

    pub fn get_u32(&self) -> Rc<Type> {
        let id = TypeId(3);
        self.map[&id].clone()
    }

    pub fn get_u64(&self) -> Rc<Type> {
        let id = TypeId(4);
        self.map[&id].clone()
    }

    pub fn get_i8(&self) -> Rc<Type> {
        let id = TypeId(5);
        self.map[&id].clone()
    }

    pub fn get_i16(&self) -> Rc<Type> {
        let id = TypeId(6);
        self.map[&id].clone()
    }

    pub fn get_i32(&self) -> Rc<Type> {
        let id = TypeId(7);
        self.map[&id].clone()
    }

    pub fn get_i64(&self) -> Rc<Type> {
        let id = TypeId(8);
        self.map[&id].clone()
    }

    pub fn get_f32(&self) -> Rc<Type> {
        let id = TypeId(9);
        self.map[&id].clone()
    }

    pub fn get_f64(&self) -> Rc<Type> {
        let id = TypeId(10);
        self.map[&id].clone()
    }

    pub fn get_bool(&self) -> Rc<Type> {
        let id = TypeId(11);
        self.map[&id].clone()
    }

    pub fn get_char(&self) -> Rc<Type> {
        let id = TypeId(12);
        self.map[&id].clone()
    }

    pub fn get_string(&self) -> Rc<Type> {
        let id = TypeId(12);
        self.map[&id].clone()
    }

    pub fn get_invalid(&self) -> Rc<Type> {
        let id = TypeId(0);
        self.map[&id].clone()
    }

    pub fn get_unit(&self) -> Rc<Type> {
        let id = TypeId(13);
        self.map[&id].clone()
    }
}
