use lazy_static::lazy_static;
use std::collections::HashMap;

use crate::ast::{AddressingMode, Mnemonic};

/// A mapper between instruction definitions and opcodes.
#[derive(Debug)]
pub struct OpcodeMapping {
    forward_map: HashMap<(Mnemonic, AddressingMode), u8>,
    reverse_map: HashMap<u8, (Mnemonic, AddressingMode)>,
}

impl OpcodeMapping {
    /// Find the opcode corresponding to the given instruction.
    #[tracing::instrument]
    pub fn find_opcode(&self, instruction: (Mnemonic, AddressingMode)) -> Option<u8> {
        self.forward_map.get(&instruction).copied()
    }

    /// Find the instruction corresponding to the given opcode.
    #[tracing::instrument]
    pub fn find_instruction(&self, opcode: u8) -> Option<(Mnemonic, AddressingMode)> {
        self.reverse_map.get(&opcode).copied()
    }

    #[tracing::instrument]
    fn new() -> Self {
        let mut forward_map = HashMap::new();
        let mut reverse_map = HashMap::new();

        let mappings = vec![
            (Mnemonic::ADC, AddressingMode::Immediate, 0x69),
            (Mnemonic::ADC, AddressingMode::ZeroPage, 0x65),
            (Mnemonic::ADC, AddressingMode::ZeroPageX, 0x75),
            (Mnemonic::ADC, AddressingMode::Absolute, 0x6D),
            (Mnemonic::ADC, AddressingMode::AbsoluteX, 0x7D),
            (Mnemonic::ADC, AddressingMode::AbsoluteY, 0x79),
            (Mnemonic::ADC, AddressingMode::IndirectIndexedX, 0x61),
            (Mnemonic::ADC, AddressingMode::IndirectIndexedY, 0x71),
            (Mnemonic::AND, AddressingMode::Immediate, 0x29),
            (Mnemonic::AND, AddressingMode::ZeroPage, 0x25),
            (Mnemonic::AND, AddressingMode::ZeroPageX, 0x35),
            (Mnemonic::AND, AddressingMode::Absolute, 0x2D),
            (Mnemonic::AND, AddressingMode::AbsoluteX, 0x3D),
            (Mnemonic::AND, AddressingMode::AbsoluteY, 0x39),
            (Mnemonic::AND, AddressingMode::IndirectIndexedX, 0x21),
            (Mnemonic::AND, AddressingMode::IndirectIndexedY, 0x31),
            (Mnemonic::ASL, AddressingMode::Accumulator, 0x0A),
            (Mnemonic::ASL, AddressingMode::ZeroPage, 0x06),
            (Mnemonic::ASL, AddressingMode::ZeroPageX, 0x16),
            (Mnemonic::ASL, AddressingMode::Absolute, 0x0E),
            (Mnemonic::ASL, AddressingMode::AbsoluteX, 0x1E),
            (Mnemonic::BCC, AddressingMode::Relative, 0x90),
            (Mnemonic::BCS, AddressingMode::Relative, 0xB0),
            (Mnemonic::BEQ, AddressingMode::Relative, 0xF0),
            (Mnemonic::BIT, AddressingMode::ZeroPage, 0x24),
            (Mnemonic::BIT, AddressingMode::Absolute, 0x2C),
            (Mnemonic::BMI, AddressingMode::Relative, 0x30),
            (Mnemonic::BNE, AddressingMode::Relative, 0xD0),
            (Mnemonic::BPL, AddressingMode::Relative, 0x10),
            (Mnemonic::BRK, AddressingMode::Implied, 0x00),
            (Mnemonic::BVC, AddressingMode::Relative, 0x50),
            (Mnemonic::BVS, AddressingMode::Relative, 0x70),
            (Mnemonic::CLC, AddressingMode::Implied, 0x18),
            (Mnemonic::CLD, AddressingMode::Implied, 0xD8),
            (Mnemonic::CLI, AddressingMode::Implied, 0x58),
            (Mnemonic::CLV, AddressingMode::Implied, 0xB8),
            (Mnemonic::CMP, AddressingMode::Immediate, 0xC9),
            (Mnemonic::CMP, AddressingMode::ZeroPage, 0xC5),
            (Mnemonic::CMP, AddressingMode::ZeroPageX, 0xD5),
            (Mnemonic::CMP, AddressingMode::Absolute, 0xCD),
            (Mnemonic::CMP, AddressingMode::AbsoluteX, 0xDD),
            (Mnemonic::CMP, AddressingMode::AbsoluteY, 0xD9),
            (Mnemonic::CMP, AddressingMode::IndirectIndexedX, 0xC1),
            (Mnemonic::CMP, AddressingMode::IndirectIndexedY, 0xD1),
            (Mnemonic::CPX, AddressingMode::Immediate, 0xE0),
            (Mnemonic::CPX, AddressingMode::ZeroPage, 0xE4),
            (Mnemonic::CPX, AddressingMode::Absolute, 0xEC),
            (Mnemonic::CPY, AddressingMode::Immediate, 0xC0),
            (Mnemonic::CPY, AddressingMode::ZeroPage, 0xC4),
            (Mnemonic::CPY, AddressingMode::Absolute, 0xCC),
            (Mnemonic::DEC, AddressingMode::ZeroPage, 0xC6),
            (Mnemonic::DEC, AddressingMode::ZeroPageX, 0xD6),
            (Mnemonic::DEC, AddressingMode::Absolute, 0xCE),
            (Mnemonic::DEC, AddressingMode::AbsoluteX, 0xDE),
            (Mnemonic::DEX, AddressingMode::Implied, 0xCA),
            (Mnemonic::DEY, AddressingMode::Implied, 0x88),
            (Mnemonic::EOR, AddressingMode::Immediate, 0x49),
            (Mnemonic::EOR, AddressingMode::ZeroPage, 0x45),
            (Mnemonic::EOR, AddressingMode::ZeroPageX, 0x55),
            (Mnemonic::EOR, AddressingMode::Absolute, 0x4D),
            (Mnemonic::EOR, AddressingMode::AbsoluteX, 0x5D),
            (Mnemonic::EOR, AddressingMode::AbsoluteY, 0x59),
            (Mnemonic::EOR, AddressingMode::IndirectIndexedX, 0x41),
            (Mnemonic::EOR, AddressingMode::IndirectIndexedY, 0x51),
            (Mnemonic::INC, AddressingMode::ZeroPage, 0xE6),
            (Mnemonic::INC, AddressingMode::ZeroPageX, 0xF6),
            (Mnemonic::INC, AddressingMode::Absolute, 0xEE),
            (Mnemonic::INC, AddressingMode::AbsoluteX, 0xFE),
            (Mnemonic::INX, AddressingMode::Implied, 0xE8),
            (Mnemonic::INY, AddressingMode::Implied, 0xC8),
            (Mnemonic::JMP, AddressingMode::Absolute, 0x4C),
            (Mnemonic::JMP, AddressingMode::Indirect, 0x6C),
            (Mnemonic::JSR, AddressingMode::Absolute, 0x20),
            (Mnemonic::LDA, AddressingMode::Immediate, 0xA9),
            (Mnemonic::LDA, AddressingMode::ZeroPage, 0xA5),
            (Mnemonic::LDA, AddressingMode::ZeroPageX, 0xB5),
            (Mnemonic::LDA, AddressingMode::Absolute, 0xAD),
            (Mnemonic::LDA, AddressingMode::AbsoluteX, 0xBD),
            (Mnemonic::LDA, AddressingMode::AbsoluteY, 0xB9),
            (Mnemonic::LDA, AddressingMode::IndirectIndexedX, 0xA1),
            (Mnemonic::LDA, AddressingMode::IndirectIndexedY, 0xB1),
            (Mnemonic::LDX, AddressingMode::Immediate, 0xA2),
            (Mnemonic::LDX, AddressingMode::ZeroPage, 0xA6),
            (Mnemonic::LDX, AddressingMode::ZeroPageY, 0xB6),
            (Mnemonic::LDX, AddressingMode::Absolute, 0xAE),
            (Mnemonic::LDX, AddressingMode::AbsoluteY, 0xBE),
            (Mnemonic::LDY, AddressingMode::Immediate, 0xA0),
            (Mnemonic::LDY, AddressingMode::ZeroPage, 0xA4),
            (Mnemonic::LDY, AddressingMode::ZeroPageX, 0xB4),
            (Mnemonic::LDY, AddressingMode::Absolute, 0xAC),
            (Mnemonic::LDY, AddressingMode::AbsoluteX, 0xBC),
            (Mnemonic::LSR, AddressingMode::Accumulator, 0x4A),
            (Mnemonic::LSR, AddressingMode::ZeroPage, 0x46),
            (Mnemonic::LSR, AddressingMode::ZeroPageX, 0x56),
            (Mnemonic::LSR, AddressingMode::Absolute, 0x4E),
            (Mnemonic::LSR, AddressingMode::AbsoluteX, 0x5E),
            (Mnemonic::NOP, AddressingMode::Implied, 0xEA),
            (Mnemonic::ORA, AddressingMode::Immediate, 0x09),
            (Mnemonic::ORA, AddressingMode::ZeroPage, 0x05),
            (Mnemonic::ORA, AddressingMode::ZeroPageX, 0x15),
            (Mnemonic::ORA, AddressingMode::Absolute, 0x0D),
            (Mnemonic::ORA, AddressingMode::AbsoluteX, 0x1D),
            (Mnemonic::ORA, AddressingMode::AbsoluteY, 0x19),
            (Mnemonic::ORA, AddressingMode::IndirectIndexedX, 0x01),
            (Mnemonic::ORA, AddressingMode::IndirectIndexedY, 0x11),
            (Mnemonic::PHA, AddressingMode::Implied, 0x48),
            (Mnemonic::PHP, AddressingMode::Implied, 0x08),
            (Mnemonic::PLA, AddressingMode::Implied, 0x68),
            (Mnemonic::PLP, AddressingMode::Implied, 0x28),
            (Mnemonic::ROL, AddressingMode::Accumulator, 0x2A),
            (Mnemonic::ROL, AddressingMode::ZeroPage, 0x26),
            (Mnemonic::ROL, AddressingMode::ZeroPageX, 0x36),
            (Mnemonic::ROL, AddressingMode::Absolute, 0x2E),
            (Mnemonic::ROL, AddressingMode::AbsoluteX, 0x3E),
            (Mnemonic::ROR, AddressingMode::Accumulator, 0x6A),
            (Mnemonic::ROR, AddressingMode::ZeroPage, 0x66),
            (Mnemonic::ROR, AddressingMode::ZeroPageX, 0x76),
            (Mnemonic::ROR, AddressingMode::Absolute, 0x6E),
            (Mnemonic::ROR, AddressingMode::AbsoluteX, 0x7E),
            (Mnemonic::RTI, AddressingMode::Implied, 0x40),
            (Mnemonic::RTS, AddressingMode::Implied, 0x60),
            (Mnemonic::SBC, AddressingMode::Immediate, 0xE9),
            (Mnemonic::SBC, AddressingMode::ZeroPage, 0xE5),
            (Mnemonic::SBC, AddressingMode::ZeroPageX, 0xF5),
            (Mnemonic::SBC, AddressingMode::Absolute, 0xED),
            (Mnemonic::SBC, AddressingMode::AbsoluteX, 0xFD),
            (Mnemonic::SBC, AddressingMode::AbsoluteY, 0xF9),
            (Mnemonic::SBC, AddressingMode::IndirectIndexedX, 0xE1),
            (Mnemonic::SBC, AddressingMode::IndirectIndexedY, 0xF1),
            (Mnemonic::SEC, AddressingMode::Implied, 0x38),
            (Mnemonic::SED, AddressingMode::Implied, 0xF8),
            (Mnemonic::SEI, AddressingMode::Implied, 0x78),
            (Mnemonic::STA, AddressingMode::ZeroPage, 0x85),
            (Mnemonic::STA, AddressingMode::ZeroPageX, 0x95),
            (Mnemonic::STA, AddressingMode::Absolute, 0x8D),
            (Mnemonic::STA, AddressingMode::AbsoluteX, 0x9D),
            (Mnemonic::STA, AddressingMode::AbsoluteY, 0x99),
            (Mnemonic::STA, AddressingMode::IndirectIndexedX, 0x81),
            (Mnemonic::STA, AddressingMode::IndirectIndexedY, 0x91),
            (Mnemonic::STX, AddressingMode::ZeroPage, 0x86),
            (Mnemonic::STX, AddressingMode::ZeroPageY, 0x96),
            (Mnemonic::STX, AddressingMode::Absolute, 0x8E),
            (Mnemonic::STY, AddressingMode::ZeroPage, 0x84),
            (Mnemonic::STY, AddressingMode::ZeroPageX, 0x94),
            (Mnemonic::STY, AddressingMode::Absolute, 0x8C),
            (Mnemonic::TAX, AddressingMode::Implied, 0xAA),
            (Mnemonic::TAY, AddressingMode::Implied, 0xA8),
            (Mnemonic::TSX, AddressingMode::Implied, 0xBA),
            (Mnemonic::TXA, AddressingMode::Implied, 0x8A),
            (Mnemonic::TXS, AddressingMode::Implied, 0x9A),
            (Mnemonic::TYA, AddressingMode::Implied, 0x98),
        ];

        for &(mnemonic, addr_mode, opcode) in &mappings {
            forward_map.insert((mnemonic, addr_mode), opcode);
            reverse_map.insert(opcode, (mnemonic, addr_mode));
        }

        OpcodeMapping {
            forward_map,
            reverse_map,
        }
    }
}

lazy_static! {
    pub static ref OPCODE_MAPPING: OpcodeMapping = OpcodeMapping::new();
}
