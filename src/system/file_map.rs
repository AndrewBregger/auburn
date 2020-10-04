use crate::system::file::{File, FileId, Path, PathBuf};
use std::collections::BTreeMap;

pub struct FileMap {
    paths: BTreeMap<PathBuf, FileId>,
    files: BTreeMap<FileId, File>,
}

impl FileMap {
    pub fn new() -> Self {
        Self {
            paths: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }

    pub fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<&File, std::io::Error> {
        let file = File::open(path.as_ref())?;
        let id = file.id();
        self.paths.insert(path.as_ref().to_path_buf(), id);
        self.files.insert(id, file);
        Ok(self.files.get(&id).unwrap())
    }

    pub fn file_by_path<P: AsRef<Path>>(&self, path: P) -> Option<&File> {
        if let Some(id) = self.paths.get(&path.as_ref().to_path_buf()) {
            self.file_by_id(id)
        } else {
            None
        }
    }

    pub fn file_by_id(&self, id: &FileId) -> Option<&File> {
        self.files.get(id)
    }
}
