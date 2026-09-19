use super::Device;

#[derive(Debug)]
pub struct Memory {
    data: Vec<u8>,
    size: usize,
    name: &'static str,
}

impl Memory {
    pub fn new(size: usize, name: &'static str) -> Self {
        Self {
            data: vec![0; size as usize],
            size: size as usize,
            name,
        }
    }
}

impl Device for Memory {
    fn name(&self) -> &str {
        self.name
    }

    fn read(&mut self, address: u16) -> u8 {
        self.data.get(address as usize).copied().unwrap_or(0)
    }

    fn write(&mut self, address: u16, value: u8) {
        if address as usize >= self.size {
            panic!(
                "Attempted to write to address {:#06x} which is out of bounds for device {}",
                address, self.name
            );
        }
        self.data
            .get_mut(address as usize)
            .map(|byte| *byte = value);
    }

    fn peek(&self, address: u16) -> u8 {
        self.data.get(address as usize).copied().unwrap_or(0)
    }
}
