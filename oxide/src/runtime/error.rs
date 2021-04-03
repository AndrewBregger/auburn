use crate::OxString;

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(
        "attempting to execute module '{}' without an entry function",
        module_name
    )]
    MissingModuleEntry { module_name: OxString },

    #[error("attempting to call a value that cannot be called: {0}")]
    CallingInvalidValue(String),
}

impl Error {
    pub fn missing_module_entry(module_name: OxString) -> Self {
        Self::MissingModuleEntry { module_name }
    }
}
