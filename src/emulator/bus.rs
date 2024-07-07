mod ram;

pub use ram::Ram;

pub trait Writeable {
    fn data_mut(&mut self) -> &mut [u8];

    fn load(&mut self, start_address: u16, data: &[u8]) {
        let mut address = start_address;
        for byte in data {
            self.write_byte(address, *byte);
            address += 1;
        }
    }
    fn write_byte(&mut self, address: u16, data: u8) {
        self.data_mut()[address as usize] = data;
    }
    fn write_word(&mut self, address: u16, data: u16) {
        self.data_mut()[address as usize] = (data & 0xff) as u8;
        self.data_mut()[(address + 1) as usize] = ((data >> 8) & 0xff) as u8;
    }
}

pub trait Readable {
    fn data(&self) -> &[u8];
    fn dump(&self, start_address: u16, end_address: u16) {
        for address in start_address..end_address {
            eprintln!("{:#06x}: {:#04x}", address, self.read_byte(address));
        }
    }

    fn read_byte(&self, address: u16) -> u8 {
        self.data()[address as usize]
    }

    /// Reads a 16-bit word from memory.
    /// The 16-bit word is stored in little-endian format.
    fn read_word(&self, address: u16) -> u16 {
        self.data()[address as usize] as u16 | ((self.data()[(address + 1) as usize] as u16) << 8)
    }
}

#[derive(Debug)]
pub struct Bus {
    ram: Ram,
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}

impl Bus {
    pub const BUS_SIZE: usize = 64 * 1024;

    #[tracing::instrument]
    pub fn new() -> Self {
        Self { ram: Ram::new() }
    }

    pub fn load(&mut self, start_address: u16, data: &[u8]) {
        self.ram.load(start_address, data);
    }
}

impl Readable for Bus {
    fn data(&self) -> &[u8] {
        self.ram.data()
    }
}

impl Writeable for Bus {
    fn data_mut(&mut self) -> &mut [u8] {
        self.ram.data_mut()
    }
}
