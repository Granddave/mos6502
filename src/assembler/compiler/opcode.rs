use lazy_static::lazy_static;
use std::collections::HashMap;

use crate::ast::{ASTAddressingMode, ASTMnemonic};

/// A mapper between ASTInstructions and opcodes.
#[derive(Debug)]
pub struct OpcodeMapping {
    forward_map: HashMap<(ASTMnemonic, ASTAddressingMode), u8>,
    reverse_map: HashMap<u8, (ASTMnemonic, ASTAddressingMode)>,
}

impl OpcodeMapping {
    /// Find the opcode corresponding to the given instruction.
    #[tracing::instrument]
    pub fn find_opcode(&self, instruction: (ASTMnemonic, ASTAddressingMode)) -> Option<u8> {
        self.forward_map.get(&instruction).copied()
    }

    /// Find the instruction corresponding to the given opcode.
    #[tracing::instrument]
    pub fn find_instruction(&self, opcode: u8) -> Option<(ASTMnemonic, ASTAddressingMode)> {
        self.reverse_map.get(&opcode).copied()
    }

    #[tracing::instrument]
    fn new() -> Self {
        let mut forward_map = HashMap::new();
        let mut reverse_map = HashMap::new();

        let mappings = vec![
            (ASTMnemonic::ADC, ASTAddressingMode::Immediate, 0x69),
            (ASTMnemonic::ADC, ASTAddressingMode::ZeroPage, 0x65),
            (ASTMnemonic::ADC, ASTAddressingMode::ZeroPageX, 0x75),
            (ASTMnemonic::ADC, ASTAddressingMode::Absolute, 0x6D),
            (ASTMnemonic::ADC, ASTAddressingMode::AbsoluteX, 0x7D),
            (ASTMnemonic::ADC, ASTAddressingMode::AbsoluteY, 0x79),
            (ASTMnemonic::ADC, ASTAddressingMode::IndirectIndexedX, 0x61),
            (ASTMnemonic::ADC, ASTAddressingMode::IndirectIndexedY, 0x71),
            (ASTMnemonic::AND, ASTAddressingMode::Immediate, 0x29),
            (ASTMnemonic::AND, ASTAddressingMode::ZeroPage, 0x25),
            (ASTMnemonic::AND, ASTAddressingMode::ZeroPageX, 0x35),
            (ASTMnemonic::AND, ASTAddressingMode::Absolute, 0x2D),
            (ASTMnemonic::AND, ASTAddressingMode::AbsoluteX, 0x3D),
            (ASTMnemonic::AND, ASTAddressingMode::AbsoluteY, 0x39),
            (ASTMnemonic::AND, ASTAddressingMode::IndirectIndexedX, 0x21),
            (ASTMnemonic::AND, ASTAddressingMode::IndirectIndexedY, 0x31),
            (ASTMnemonic::ASL, ASTAddressingMode::Accumulator, 0x0A),
            (ASTMnemonic::ASL, ASTAddressingMode::ZeroPage, 0x06),
            (ASTMnemonic::ASL, ASTAddressingMode::ZeroPageX, 0x16),
            (ASTMnemonic::ASL, ASTAddressingMode::Absolute, 0x0E),
            (ASTMnemonic::ASL, ASTAddressingMode::AbsoluteX, 0x1E),
            (ASTMnemonic::BCC, ASTAddressingMode::Relative, 0x90),
            (ASTMnemonic::BCS, ASTAddressingMode::Relative, 0xB0),
            (ASTMnemonic::BEQ, ASTAddressingMode::Relative, 0xF0),
            (ASTMnemonic::BIT, ASTAddressingMode::ZeroPage, 0x24),
            (ASTMnemonic::BIT, ASTAddressingMode::Absolute, 0x2C),
            (ASTMnemonic::BMI, ASTAddressingMode::Relative, 0x30),
            (ASTMnemonic::BNE, ASTAddressingMode::Relative, 0xD0),
            (ASTMnemonic::BPL, ASTAddressingMode::Relative, 0x10),
            (ASTMnemonic::BRK, ASTAddressingMode::Implied, 0x00),
            (ASTMnemonic::BVC, ASTAddressingMode::Relative, 0x50),
            (ASTMnemonic::BVS, ASTAddressingMode::Relative, 0x70),
            (ASTMnemonic::CLC, ASTAddressingMode::Implied, 0x18),
            (ASTMnemonic::CLD, ASTAddressingMode::Implied, 0xD8),
            (ASTMnemonic::CLI, ASTAddressingMode::Implied, 0x58),
            (ASTMnemonic::CLV, ASTAddressingMode::Implied, 0xB8),
            (ASTMnemonic::CMP, ASTAddressingMode::Immediate, 0xC9),
            (ASTMnemonic::CMP, ASTAddressingMode::ZeroPage, 0xC5),
            (ASTMnemonic::CMP, ASTAddressingMode::ZeroPageX, 0xD5),
            (ASTMnemonic::CMP, ASTAddressingMode::Absolute, 0xCD),
            (ASTMnemonic::CMP, ASTAddressingMode::AbsoluteX, 0xDD),
            (ASTMnemonic::CMP, ASTAddressingMode::AbsoluteY, 0xD9),
            (ASTMnemonic::CMP, ASTAddressingMode::IndirectIndexedX, 0xC1),
            (ASTMnemonic::CMP, ASTAddressingMode::IndirectIndexedY, 0xD1),
            (ASTMnemonic::CPX, ASTAddressingMode::Immediate, 0xE0),
            (ASTMnemonic::CPX, ASTAddressingMode::ZeroPage, 0xE4),
            (ASTMnemonic::CPX, ASTAddressingMode::Absolute, 0xEC),
            (ASTMnemonic::CPY, ASTAddressingMode::Immediate, 0xC0),
            (ASTMnemonic::CPY, ASTAddressingMode::ZeroPage, 0xC4),
            (ASTMnemonic::CPY, ASTAddressingMode::Absolute, 0xCC),
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPage, 0xC6),
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPageX, 0xD6),
            (ASTMnemonic::DEC, ASTAddressingMode::Absolute, 0xCE),
            (ASTMnemonic::DEC, ASTAddressingMode::AbsoluteX, 0xDE),
            (ASTMnemonic::DEX, ASTAddressingMode::Implied, 0xCA),
            (ASTMnemonic::DEY, ASTAddressingMode::Implied, 0x88),
            (ASTMnemonic::EOR, ASTAddressingMode::Immediate, 0x49),
            (ASTMnemonic::EOR, ASTAddressingMode::ZeroPage, 0x45),
            (ASTMnemonic::EOR, ASTAddressingMode::ZeroPageX, 0x55),
            (ASTMnemonic::EOR, ASTAddressingMode::Absolute, 0x4D),
            (ASTMnemonic::EOR, ASTAddressingMode::AbsoluteX, 0x5D),
            (ASTMnemonic::EOR, ASTAddressingMode::AbsoluteY, 0x59),
            (ASTMnemonic::EOR, ASTAddressingMode::IndirectIndexedX, 0x41),
            (ASTMnemonic::EOR, ASTAddressingMode::IndirectIndexedY, 0x51),
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPage, 0xE6),
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPageX, 0xF6),
            (ASTMnemonic::INC, ASTAddressingMode::Absolute, 0xEE),
            (ASTMnemonic::INC, ASTAddressingMode::AbsoluteX, 0xFE),
            (ASTMnemonic::INX, ASTAddressingMode::Implied, 0xE8),
            (ASTMnemonic::INY, ASTAddressingMode::Implied, 0xC8),
            (ASTMnemonic::JMP, ASTAddressingMode::Absolute, 0x4C),
            (ASTMnemonic::JMP, ASTAddressingMode::Indirect, 0x6C),
            (ASTMnemonic::JSR, ASTAddressingMode::Absolute, 0x20),
            (ASTMnemonic::LDA, ASTAddressingMode::Immediate, 0xA9),
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPage, 0xA5),
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPageX, 0xB5),
            (ASTMnemonic::LDA, ASTAddressingMode::Absolute, 0xAD),
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteX, 0xBD),
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteY, 0xB9),
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedX, 0xA1),
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedY, 0xB1),
            (ASTMnemonic::LDX, ASTAddressingMode::Immediate, 0xA2),
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPage, 0xA6),
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPageY, 0xB6),
            (ASTMnemonic::LDX, ASTAddressingMode::Absolute, 0xAE),
            (ASTMnemonic::LDX, ASTAddressingMode::AbsoluteY, 0xBE),
            (ASTMnemonic::LDY, ASTAddressingMode::Immediate, 0xA0),
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPage, 0xA4),
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPageX, 0xB4),
            (ASTMnemonic::LDY, ASTAddressingMode::Absolute, 0xAC),
            (ASTMnemonic::LDY, ASTAddressingMode::AbsoluteX, 0xBC),
            (ASTMnemonic::LSR, ASTAddressingMode::Accumulator, 0x4A),
            (ASTMnemonic::LSR, ASTAddressingMode::ZeroPage, 0x46),
            (ASTMnemonic::LSR, ASTAddressingMode::ZeroPageX, 0x56),
            (ASTMnemonic::LSR, ASTAddressingMode::Absolute, 0x4E),
            (ASTMnemonic::LSR, ASTAddressingMode::AbsoluteX, 0x5E),
            (ASTMnemonic::NOP, ASTAddressingMode::Implied, 0xEA),
            (ASTMnemonic::ORA, ASTAddressingMode::Immediate, 0x09),
            (ASTMnemonic::ORA, ASTAddressingMode::ZeroPage, 0x05),
            (ASTMnemonic::ORA, ASTAddressingMode::ZeroPageX, 0x15),
            (ASTMnemonic::ORA, ASTAddressingMode::Absolute, 0x0D),
            (ASTMnemonic::ORA, ASTAddressingMode::AbsoluteX, 0x1D),
            (ASTMnemonic::ORA, ASTAddressingMode::AbsoluteY, 0x19),
            (ASTMnemonic::ORA, ASTAddressingMode::IndirectIndexedX, 0x01),
            (ASTMnemonic::ORA, ASTAddressingMode::IndirectIndexedY, 0x11),
            (ASTMnemonic::PHA, ASTAddressingMode::Implied, 0x48),
            (ASTMnemonic::PHP, ASTAddressingMode::Implied, 0x08),
            (ASTMnemonic::PLA, ASTAddressingMode::Implied, 0x68),
            (ASTMnemonic::PLP, ASTAddressingMode::Implied, 0x28),
            (ASTMnemonic::ROL, ASTAddressingMode::Accumulator, 0x2A),
            (ASTMnemonic::ROL, ASTAddressingMode::ZeroPage, 0x26),
            (ASTMnemonic::ROL, ASTAddressingMode::ZeroPageX, 0x36),
            (ASTMnemonic::ROL, ASTAddressingMode::Absolute, 0x2E),
            (ASTMnemonic::ROL, ASTAddressingMode::AbsoluteX, 0x3E),
            (ASTMnemonic::ROR, ASTAddressingMode::Accumulator, 0x6A),
            (ASTMnemonic::ROR, ASTAddressingMode::ZeroPage, 0x66),
            (ASTMnemonic::ROR, ASTAddressingMode::ZeroPageX, 0x76),
            (ASTMnemonic::ROR, ASTAddressingMode::Absolute, 0x6E),
            (ASTMnemonic::ROR, ASTAddressingMode::AbsoluteX, 0x7E),
            (ASTMnemonic::RTI, ASTAddressingMode::Implied, 0x40),
            (ASTMnemonic::RTS, ASTAddressingMode::Implied, 0x60),
            (ASTMnemonic::SBC, ASTAddressingMode::Immediate, 0xE9),
            (ASTMnemonic::SBC, ASTAddressingMode::ZeroPage, 0xE5),
            (ASTMnemonic::SBC, ASTAddressingMode::ZeroPageX, 0xF5),
            (ASTMnemonic::SBC, ASTAddressingMode::Absolute, 0xED),
            (ASTMnemonic::SBC, ASTAddressingMode::AbsoluteX, 0xFD),
            (ASTMnemonic::SBC, ASTAddressingMode::AbsoluteY, 0xF9),
            (ASTMnemonic::SBC, ASTAddressingMode::IndirectIndexedX, 0xE1),
            (ASTMnemonic::SBC, ASTAddressingMode::IndirectIndexedY, 0xF1),
            (ASTMnemonic::SEC, ASTAddressingMode::Implied, 0x38),
            (ASTMnemonic::SED, ASTAddressingMode::Implied, 0xF8),
            (ASTMnemonic::SEI, ASTAddressingMode::Implied, 0x78),
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPage, 0x85),
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPageX, 0x95),
            (ASTMnemonic::STA, ASTAddressingMode::Absolute, 0x8D),
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteX, 0x9D),
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteY, 0x99),
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedX, 0x81),
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedY, 0x91),
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPage, 0x86),
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPageY, 0x96),
            (ASTMnemonic::STX, ASTAddressingMode::Absolute, 0x8E),
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPage, 0x84),
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPageX, 0x94),
            (ASTMnemonic::STY, ASTAddressingMode::Absolute, 0x8C),
            (ASTMnemonic::TAX, ASTAddressingMode::Implied, 0xAA),
            (ASTMnemonic::TAY, ASTAddressingMode::Implied, 0xA8),
            (ASTMnemonic::TSX, ASTAddressingMode::Implied, 0xBA),
            (ASTMnemonic::TXA, ASTAddressingMode::Implied, 0x8A),
            (ASTMnemonic::TXS, ASTAddressingMode::Implied, 0x9A),
            (ASTMnemonic::TYA, ASTAddressingMode::Implied, 0x98),
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
    /// A mapping between ASTInstructions and opcodes.
    pub static ref OPCODE_MAPPING: OpcodeMapping = OpcodeMapping::new();
}
