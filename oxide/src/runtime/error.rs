use crate::OxString;

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("attempting to execute module '{}' without an entry function", module_name)]
    MissingModuleEntry {
        module_name: OxString,
    }
}


impl Error {
    pub fn missing_module_entry(module_name: OxString) -> Self {
        Self::MissingModuleEntry {
            module_name
        }
    }
}

