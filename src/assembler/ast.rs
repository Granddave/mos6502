use std::fmt;

use strum_macros::EnumString;

pub mod instruction;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, EnumString)]
pub enum ASTMnemonic {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

impl ASTMnemonic {
    pub fn is_branch(&self) -> bool {
        matches!(
            self,
            ASTMnemonic::BCC
                | ASTMnemonic::BCS
                | ASTMnemonic::BEQ
                | ASTMnemonic::BMI
                | ASTMnemonic::BNE
                | ASTMnemonic::BPL
                | ASTMnemonic::BVC
                | ASTMnemonic::BVS
        )
    }

    pub fn is_implied(&self) -> bool {
        matches!(
            self,
            ASTMnemonic::BRK
                | ASTMnemonic::CLC
                | ASTMnemonic::CLD
                | ASTMnemonic::CLI
                | ASTMnemonic::CLV
                | ASTMnemonic::DEX
                | ASTMnemonic::DEY
                | ASTMnemonic::INX
                | ASTMnemonic::INY
                | ASTMnemonic::NOP
                | ASTMnemonic::PHA
                | ASTMnemonic::PHP
                | ASTMnemonic::PLA
                | ASTMnemonic::PLP
                | ASTMnemonic::RTI
                | ASTMnemonic::RTS
                | ASTMnemonic::SEC
                | ASTMnemonic::SED
                | ASTMnemonic::SEI
                | ASTMnemonic::TAX
                | ASTMnemonic::TAY
                | ASTMnemonic::TSX
                | ASTMnemonic::TXA
                | ASTMnemonic::TXS
                | ASTMnemonic::TYA
        )
    }
}

impl fmt::Display for ASTMnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  {:?}", self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub enum ASTAddressingMode {
    // Addressing modes
    Absolute,         // a
    ZeroPage,         // zp
    ZeroPageX,        // zp,x
    ZeroPageY,        // zp,y
    AbsoluteX,        // a,x
    AbsoluteY,        // a,y
    Relative,         // r, for branch instructions (Labels are resolved to relative offsets)
    Indirect,         // (a)
    IndirectIndexedX, // (zp,x)
    IndirectIndexedY, // (zp),y
    // Direct and immediate operands
    Immediate, // #v
    Accumulator,
    Implied,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum ASTOperand {
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    Relative(i8),
    Label(String),
    Implied,
}

/// An instruction without an operand
#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct ASTInstruction {
    /// Mnemonic of the instruction
    pub mnemonic: ASTMnemonic,
    /// Combined addressing mode and operand
    pub addr_mode: ASTAddressingMode,
}

impl ASTInstruction {
    pub fn to_opcode(&self) -> u8 {
        instruction::OPCODE_MAPPING.find_opcode(*self).unwrap()
    }

    pub fn from_opcode(opcode: u8) -> Option<Self> {
        instruction::OPCODE_MAPPING.find_instruction(opcode)
    }
}

/// An instruction with an operand.
#[derive(Debug, PartialEq)]
pub struct ASTInstructionNode {
    /// The first part of the instruction
    pub ins: ASTInstruction,
    /// Operand of the instruction
    pub operand: ASTOperand,
}

impl ASTInstructionNode {
    pub fn new(
        mnemonic: ASTMnemonic,
        addr_mode: ASTAddressingMode,
        operand: ASTOperand,
    ) -> ASTInstructionNode {
        ASTInstructionNode {
            ins: ASTInstruction {
                mnemonic,
                addr_mode,
            },
            operand,
        }
    }

    /// Size of instruction and operand in bytes
    pub fn size(&self) -> usize {
        match self.ins.addr_mode {
            ASTAddressingMode::Absolute
            | ASTAddressingMode::AbsoluteX
            | ASTAddressingMode::AbsoluteY
            | ASTAddressingMode::Indirect
            | ASTAddressingMode::IndirectIndexedX
            | ASTAddressingMode::IndirectIndexedY => 3,
            ASTAddressingMode::ZeroPage
            | ASTAddressingMode::ZeroPageX
            | ASTAddressingMode::ZeroPageY
            | ASTAddressingMode::Immediate
            | ASTAddressingMode::Relative => 2,
            ASTAddressingMode::Implied | ASTAddressingMode::Accumulator => 1,
        }
    }
}

impl fmt::Display for ASTInstructionNode {
    // NOTE: This might not be correct for all addressing modes
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ins.addr_mode == ASTAddressingMode::Implied {
            write!(f, "{}", self.ins.mnemonic)
        } else {
            match &self.operand {
                ASTOperand::Immediate(value) => match self.ins.addr_mode {
                    ASTAddressingMode::Immediate => {
                        write!(f, "{} #${:02X}", self.ins.mnemonic, value)
                    }
                    ASTAddressingMode::ZeroPage => {
                        write!(f, "{} ${:02X}", self.ins.mnemonic, value)
                    }
                    _ => write!(f, "{} #${:02X}", self.ins.mnemonic, value),
                },
                ASTOperand::Absolute(address) => match self.ins.addr_mode {
                    ASTAddressingMode::Absolute => {
                        write!(f, "{} ${:04X}", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::AbsoluteX => {
                        write!(f, "{} ${:04X},X", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::AbsoluteY => {
                        write!(f, "{} ${:04X},Y", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::Indirect => {
                        write!(f, "{} (${:04X})", self.ins.mnemonic, address)
                    }
                    _ => write!(f, "{} ${:04X}", self.ins.mnemonic, address),
                },
                ASTOperand::ZeroPage(address) => match self.ins.addr_mode {
                    ASTAddressingMode::ZeroPage => {
                        write!(f, "{} ${:02X}", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::ZeroPageX => {
                        write!(f, "{} ${:02X},X", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::ZeroPageY => {
                        write!(f, "{} ${:02X},Y", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::IndirectIndexedX => {
                        write!(f, "{} (${:02X},X)", self.ins.mnemonic, address)
                    }
                    ASTAddressingMode::IndirectIndexedY => {
                        write!(f, "{} (${:02X}),Y", self.ins.mnemonic, address)
                    }
                    _ => write!(f, "{} ${:02X}", self.ins.mnemonic, address),
                },
                ASTOperand::Relative(offset) => write!(f, "{} ${:02X}", self.ins.mnemonic, offset),
                ASTOperand::Label(label) => write!(f, "{} {}", self.ins.mnemonic, label),
                ASTOperand::Implied => write!(f, "{}", self.ins.mnemonic),
            }
        }
    }
}

/// AST Node is either an instruction (including operand) or a label.
#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Instruction(ASTInstructionNode),
    Label(String),
    // TODO: Add define directive
}

impl ASTNode {
    pub fn new_instruction(
        mnemonic: ASTMnemonic,
        addr_mode: ASTAddressingMode,
        operand: ASTOperand,
    ) -> ASTNode {
        ASTNode::Instruction(ASTInstructionNode::new(mnemonic, addr_mode, operand))
    }
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Instruction(instruction) => write!(f, "{}", instruction),
            ASTNode::Label(label) => write!(f, "{}:", label),
        }
    }
}
