use crate::emulator::{
    instruction::{try_decode, AddressingMode, Instruction},
    memory::Bus,
};

#[derive(Debug)]
struct Status {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal: bool,
    break_command: bool,
    overflow: bool,
    negative: bool,
}

#[derive(Debug)]
pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: Status,
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self::default()
    }

    fn fetch_and_decode(&mut self, memory: &mut dyn Bus) -> Option<(Instruction, AddressingMode)> {
        let opcode = memory.read(self.pc);
        self.pc += 1;

        try_decode(opcode)
    }

    pub fn run(&mut self, memory: &mut dyn Bus, cycles: usize) {
        while cycles > 0 {
            let instruction = self.fetch_and_decode(memory).expect("Valid opcode");

            println!("{:?}", instruction);
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    // use crate::memory::Memory;

    // #[test]
    // fn test_lda_imm() {
    //     let mut cpu = Cpu::new();
    //     let mut memory = crate::memory::Memory::new();
    //
    //     memory.load(0x0000, &[0x8D, 0x05]);
    // }
}
