use crate::{
    assembler::ast::{self},
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

    fn fetch_and_decode(&mut self, _memory: &mut dyn Bus) -> Option<ast::ASTInstructionNode> {
        // let opcode = memory.read(self.pc);
        // self.pc += 1;
        //
        // if let Some(ins) = OPCODE_MAPPING.find_instruction(opcode) {
        //     return Some(ast::ASTInstructionNode::new(
        //         ins.mnemonic,
        //         ins.addr_mode,
        //         ast::ASTOperand::Immediate(0),
        //     ));
        // }

        None
    }

    pub fn run(&mut self, _memory: &mut dyn Bus, _cycles: usize) {
        todo!();
        // while cycles > 0 {
        //     let instruction = self.fetch_and_decode(memory).expect("Valid opcode");
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
