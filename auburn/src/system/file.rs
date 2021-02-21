use std::fs::read_to_string;
pub use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use itertools::Itertools;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct FileId(pub usize);

impl FileId {
    pub fn next() -> Self {
        static TOKEN: AtomicUsize = AtomicUsize::new(1);
        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }

    pub fn invalid() -> Self {
        Self(0)
    }

    pub fn is_valid(&self) -> bool {
        self.0 > 0
    }
}

#[derive(Debug, Clone)]
pub struct File {
    content: String,
    path: PathBuf,
    fid: FileId,
    lines: Vec<String>,
}

impl File {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let path_buff = path.as_ref().to_path_buf();

        println!("File name: {}", path_buff.display());
        println!("Ext: {:?}", path_buff.extension());

        let content = read_to_string(path)?;

        let mut file = Self {
            content,
            path: path_buff,
            fid: FileId::next(),
            lines: vec![],
        };

        file.separate_lines();

        Ok(file)
    }

    fn separate_lines(&mut self) {
        self.lines = self.content.lines().map(|x| x.to_string()).collect_vec();
    }

    #[cfg(test)]
    pub fn raw_test(content: String) -> Self {
        Self {
            content,
            path: PathBuf::new(),
            fid: FileId(0),
            lines: vec![],
        }
    }

    pub fn get_line(&self, line: usize) -> &str {
        self.lines[line - 1].as_str()
    }

    pub fn get_lines(&self, start: usize, end: usize) -> Vec<&str> {
        let mut lines = Vec::with_capacity(end - start + 1);

        for line in start..(end + 1) {
            lines.push(self.get_line(line - 1));
        }

        lines
    }

    pub fn content(&self) -> &str {
        self.content.as_str()
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn id(&self) -> FileId {
        self.fid
    }

    pub fn name(&self) -> &str {
        if let Some(os_s) = self.path.file_name() {
            os_s.to_str().unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn stem(&self) -> &str {
        if let Some(os_s) = self.path.file_stem() {
            os_s.to_str().unwrap()
        } else {
            unreachable!()
        }
    }
}
