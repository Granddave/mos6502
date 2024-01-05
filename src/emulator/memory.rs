pub trait Bus {
    fn read_byte(&self, address: u16) -> u8;
    fn read_word(&self, address: u16) -> u16;
    fn write_byte(&mut self, address: u16, data: u8);
    fn write_word(&mut self, address: u16, data: u16);
}

#[derive(Debug)]
pub struct Memory {
    data: [u8; Memory::MEMORY_SIZE],
}

impl Memory {
    pub const MEMORY_SIZE: usize = 64 * 1024;

    #[tracing::instrument]
    pub fn new() -> Self {
        Self {
            data: [0; Memory::MEMORY_SIZE],
        }
    }

    /// Get a slice reference
    #[tracing::instrument]
    pub fn slice(&self, start_address: usize, end_address: usize) -> &[u8] {
        &self.data[start_address..end_address]
    }

    #[tracing::instrument]
    pub fn dump(&self, start_address: u16, end_address: u16) {
        for address in start_address..end_address {
            eprintln!("{:#06x}: {:#04x}", address, self.read_byte(address));
        }
    }

    #[tracing::instrument]
    pub fn load(&mut self, start_address: u16, data: &[u8]) {
        let mut address = start_address;
        for byte in data {
            self.write_byte(address, *byte);
            address += 1;
        }
    }
}

impl Default for Memory {
    #[tracing::instrument]
    fn default() -> Self {
        Self::new()
    }
}

impl Bus for Memory {
    /// Reads a byte from memory.
    #[tracing::instrument]
    fn read_byte(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    /// Reads a 16-bit word from memory.
    /// The 16-bit word is stored in little-endian format.
    #[tracing::instrument]
    fn read_word(&self, address: u16) -> u16 {
        self.data[address as usize] as u16 | ((self.data[(address + 1) as usize] as u16) << 8)
    }

    /// Writes a byte to memory.
    #[tracing::instrument]
    fn write_byte(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }

    #[tracing::instrument]
    fn write_word(&mut self, address: u16, data: u16) {
        self.data[address as usize] = (data & 0xff) as u8;
        self.data[(address + 1) as usize] = ((data >> 8) & 0xff) as u8;
    }
}
