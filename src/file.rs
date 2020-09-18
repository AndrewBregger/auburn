use std::fs::read_to_string;
pub use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

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

pub struct File {
    content: String,
    path: PathBuf,
    fid: FileId,
}

impl File {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let path_buff = path.as_ref().to_path_buf();
        let content = read_to_string(path)?;

        Ok(Self {
            content,
            path: path_buff,
            fid: FileId::next(),
        })
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

    pub fn file_name(&self) -> &str {
        if let Some(os_s) = self.path.file_name() {
            os_s.to_str().unwrap()
        } else {
            unreachable!()
        }
    }
}
