extern crate smallvec;
use std::fmt::{Display, Formatter, Result};

use smallvec::{smallvec, SmallVec};

#[derive(Debug, Clone)]
pub struct BitMap {
    buffer: SmallVec<[u8; 16]>,
}

impl BitMap {
    pub fn new(len: usize) -> Self {
        let rem = len % 8;
        let bytes = (len / 8) + if rem == 0 { 0 } else { 1 };
        Self {
            buffer: smallvec!(0x00; bytes),
        }
    }

    pub fn capacity(&self) -> usize {
        self.buffer.capacity() * 8
    }

    pub fn get(&self, index: usize) -> bool {
        if index > self.capacity() {
            panic!(
                "index {} is out of bounds for size {}",
                index,
                self.capacity()
            );
        }
        let idx = index / 8;
        let offset = index % 8;

        let byte = self.buffer[idx];
        let value = (byte >> offset) & 0x1;
        value == 1
    }

    pub fn set(&mut self, index: usize, value: bool) {
        let idx = index / 8;
        let offset = index % 8;

        let byte = self.buffer[idx];
        self.buffer[idx] = (byte & !(1 << offset)) | ((value as u8) << offset);
    }

    pub fn first_zero(&self) -> Option<usize> {
        for (idx, element) in self.buffer.iter().enumerate() {
            // if not all bits are set then find the first bit in this byte
            if *element < 0xff {
                // @TODO: hack
                return Some(idx * 8 + element.trailing_ones() as usize);
            }
        }
        None
    }

    pub fn count_ones(&self) -> usize {
        self.buffer.iter().map(|x| x.count_ones()).sum::<u32>() as usize
    }
}

impl Display for BitMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for element in self.buffer.iter() {
            write!(f, "{:08b} ", *element)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {}
