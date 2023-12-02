pub trait Bus {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, data: u8);
}

pub struct Memory {
    data: [u8; Memory::MEMORY_SIZE],
}

impl Memory {
    pub const MEMORY_SIZE: usize = 64 * 1024;

    pub fn new() -> Self {
        Self {
            data: [0; Memory::MEMORY_SIZE],
        }
    }

    pub fn load(&mut self, start_address: u16, data: &[u8]) {
        let mut address = start_address;
        for byte in data {
            self.write(address, *byte);
            address += 1;
        }
    }
}

impl Bus for Memory {
    fn read(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }
}
