use std::collections::BTreeMap;
use std::rc::Rc;

use crate::system::file::{File, FileId, Path, PathBuf};

#[derive(Debug, Clone)]
pub struct FileMap {
    paths: BTreeMap<PathBuf, FileId>,
    files: BTreeMap<FileId, Rc<File>>,
}

impl FileMap {
    pub fn new() -> Self {
        Self {
            paths: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }

    pub fn open_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Rc<File>, std::io::Error> {
        let file = File::open(path.as_ref())?;
        let id = file.id();
        self.paths.insert(path.as_ref().to_path_buf(), id);
        self.files.insert(id, Rc::new(file));
        println!("??{:?}", self.files);
        Ok(self.files.get(&id).unwrap().clone())
    }

    pub fn file_by_path<P: AsRef<Path>>(&self, path: P) -> Option<Rc<File>> {
        if let Some(id) = self.paths.get(&path.as_ref().to_path_buf()) {
            self.file_by_id(id)
        } else {
            None
        }
    }

    pub fn file_by_id(&self, id: &FileId) -> Option<Rc<File>> {
        self.files.get(id).map(|f| f.clone())
    }

    pub fn find(&self, id: &FileId) -> Option<&Rc<File>> {
        self.files.get(id)
    }

    //pub fn get_path_by_id(&self, id: &FileId) -> Option<&str> {
    //    self.files.get(id).map(|f| f.name())
    //}
}
