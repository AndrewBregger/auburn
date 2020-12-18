fields = [
    "I8",
    "I16",
    "I32",
    "I64",
    "U8",
    "U16",
    "U32",
    "U64",
    "F32",
    "F64",
    "Bool"]

for field in fields:
    print(f"pub fn as_{field.lower()}(&self) -> {field.lower()} {{")
    print(f"    if let Self::{field}(val) = self {{")
    print("        *val")
    print("    }")
    print("    else {")
    print(f'        panic!("Attempting to get an {field.lower()} from a value of type {{}}", self.ty());')
    print("    }")
    print("}")
