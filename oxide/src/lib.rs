mod mem;
mod runtime;
mod value;
pub mod vm;

pub use runtime::Section;
pub use value::Value;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
