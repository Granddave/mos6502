mod memory;

use memory::Memory;
use std::fmt::Debug;

pub trait Device: Debug {
    fn name(&self) -> &str;
    fn read(&mut self, offset: u16) -> u8;
    fn peek(&self, offset: u16) -> u8;
    fn write(&mut self, offset: u16, value: u8);
    // fn reset(&mut self)
}

#[derive(Debug)]
struct Mapping {
    start: u16,
    end: u16,
    device: Box<dyn Device>,
}

#[derive(Debug, Default)]
pub struct Bus {
    mappings: Vec<Mapping>,
}

impl Bus {
    pub const BUS_SIZE: usize = 64 * 1024;

    #[tracing::instrument]
    pub fn new() -> Self {
        Self {
            mappings: vec![Mapping {
                start: 0x0000,
                end: 0xffff,
                device: Box::new(Memory::new(Self::BUS_SIZE, "RAM")),
            }],
        }
    }

    fn find_mapping(&self, address: u16) -> Option<&Mapping> {
        self.mappings
            .iter()
            .find(|mapping| address >= mapping.start && address <= mapping.end)
    }

    fn find_mapping_mut(&mut self, address: u16) -> Option<&mut Mapping> {
        self.mappings
            .iter_mut()
            .find(|mapping| address >= mapping.start && address <= mapping.end)
    }
    pub fn load(&mut self, start_address: u16, data: &[u8]) {
        match self.find_mapping_mut(start_address) {
            Some(mapping) => {
                let offset = start_address - mapping.start;
                for (i, byte) in data.iter().enumerate() {
                    mapping.device.write(offset + i as u16, *byte);
                }
            }
            None => {
                eprintln!(
                    "No device mapped for address {:#06x}. Data not loaded.",
                    start_address
                );
            }
        }
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        match self.find_mapping_mut(address) {
            Some(mapping) => mapping.device.write(address - mapping.start, value),
            None => eprintln!(
                "No device mapped for address {:#06x}. Write ignored.",
                address
            ),
        }
    }

    pub fn write_word(&mut self, address: u16, value: u16) {
        self.write_byte(address, (value & 0xff) as u8);
        self.write_byte(address + 1, ((value >> 8) & 0xff) as u8);
    }

    pub fn read_byte(&mut self, address: u16) -> u8 {
        match self.find_mapping_mut(address) {
            Some(mapping) => mapping.device.read(address - mapping.start),
            None => 0,
        }
    }

    pub fn read_word(&mut self, address: u16) -> u16 {
        let low = self.read_byte(address) as u16;
        let high = self.read_byte(address + 1) as u16;
        (high << 8) | low
    }

    pub fn peek_byte(&self, address: u16) -> u8 {
        match self.find_mapping(address) {
            Some(mapping) => mapping.device.peek(address - mapping.start),
            None => 0,
        }
    }

    pub fn peek_slice(&self, start_address: u16, end_address: u16) -> Vec<u8> {
        let mut data = Vec::new();
        for address in start_address..end_address {
            data.push(self.peek_byte(address));
        }
        data
    }

    pub fn attach_device(&mut self, device: Box<dyn Device>, start_address: u16, end_address: u16) {
        self.mappings.push(Mapping {
            start: start_address,
            end: end_address,
            device,
        });
    }
}
