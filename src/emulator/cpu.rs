use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{ASTAddressingMode, ASTInstructionNode, ASTOperand},
    emulator::memory::Bus,
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

    /// Fetches and decodes the next instruction from memory.
    /// Returns the instruction and increments the program counter.
    fn fetch_and_decode(&mut self, memory: &mut dyn Bus) -> ASTInstructionNode {
        let opcode = memory.read_byte(self.pc);
        let ins = OPCODE_MAPPING
            .find_instruction(opcode)
            .unwrap_or_else(|| panic!("Invalid opcode: '{:#02x}'", opcode));

        let operand = match ins.addr_mode {
            ASTAddressingMode::Absolute
            | ASTAddressingMode::AbsoluteX
            | ASTAddressingMode::AbsoluteY
            | ASTAddressingMode::Indirect => ASTOperand::Absolute(memory.read_word(self.pc)),
            ASTAddressingMode::ZeroPage
            | ASTAddressingMode::ZeroPageX
            | ASTAddressingMode::ZeroPageY
            | ASTAddressingMode::IndirectIndexedX
            | ASTAddressingMode::IndirectIndexedY => {
                ASTOperand::ZeroPage(memory.read_byte(self.pc))
            }
            ASTAddressingMode::Relative => ASTOperand::Relative(memory.read_byte(self.pc) as i8),
            ASTAddressingMode::Immediate => ASTOperand::Immediate(memory.read_byte(self.pc)),
            ASTAddressingMode::Accumulator | ASTAddressingMode::Implied => ASTOperand::Implied,
            _ => panic!("Invalid addressing mode: '{:#?}'", ins.addr_mode),
        };

        let instruction = ASTInstructionNode { ins, operand };
        self.pc += instruction.size() as u16;
        instruction
    }

    pub fn run(&mut self, _memory: &mut dyn Bus, _cycles: usize) {
        todo!();
        // while cycles > 0 {
        //     let instruction = self.fetch_and_decode(memory)
        //
        //     println!("{:?}", instruction);
        // }
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
