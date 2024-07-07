use super::{Bus, Readable, Writeable};

#[derive(Debug)]
pub struct Ram {
    data: [u8; Bus::BUS_SIZE],
    #[allow(dead_code)] // Not used at the moment, but would be useful for memory-mapped I/O
    size: usize,
}

impl Ram {
    pub fn new() -> Self {
        Self {
            data: [0; Bus::BUS_SIZE],
            size: Bus::BUS_SIZE,
        }
    }

    pub fn _size(&self) -> usize {
        self.size
    }
}

impl Default for Ram {
    fn default() -> Self {
        Self::new()
    }
}

impl Readable for Ram {
    fn data(&self) -> &[u8] {
        &self.data
    }
}

impl Writeable for Ram {
    fn data_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }
}
