use std::fmt::{Display, Formatter};
// pub use crate::gc::{Cell, GcCell};
#[derive(Debug, Clone)]
pub struct OxString {
    // cell: Cell,
    value: String,
}

impl OxString {
    pub fn new(value: String) -> Self {
        Self { value }
    }

    pub fn str(&self) -> &str {
        self.value.as_str()
    }
}

impl Display for OxString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<&str> for OxString {
    fn from(other: &str) -> Self {
        Self::new(other.to_owned())
    }
}

impl From<String> for OxString {
    fn from(other: String) -> Self {
        Self::new(other)
    }
}
