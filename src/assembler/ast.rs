use std::fmt;

use strum_macros::EnumString;

#[derive(Debug, PartialEq, EnumString)]
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

#[derive(Debug, PartialEq)]
pub enum ASTOperand {
    // Addressing modes
    Absolute(u16),        // a
    ZeroPage(u8),         // zp
    ZeroPageX(u8),        // zp,x
    ZeroPageY(u8),        // zp,y
    AbsoluteX(u16),       // a,x
    AbsoluteY(u16),       // a,y
    Relative(i8),         // r, for branch instructions
    Label(String),        // To be converted to a relative address
    Indirect(u16),        // (a)
    IndirectIndexedX(u8), // (zp,x)
    IndirectIndexedY(u8), // (zp),y
    // Direct and immediate operands
    Immediate(u8), // #v
    Implied,
    // Accumulator, // Implied operand. Needed or part of Implied?
}

impl fmt::Display for ASTOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTOperand::Absolute(address) => write!(f, "${:04X}", address),
            ASTOperand::ZeroPage(address) => write!(f, "${:02X}", address),
            ASTOperand::ZeroPageX(address) => write!(f, "${:02X},X", address),
            ASTOperand::ZeroPageY(address) => write!(f, "${:02X},Y", address),
            ASTOperand::AbsoluteX(address) => write!(f, "${:04X},X", address),
            ASTOperand::AbsoluteY(address) => write!(f, "${:04X},Y", address),
            ASTOperand::Relative(offset) => write!(f, "${:02X}", offset),
            ASTOperand::Label(label) => write!(f, "{}", label),
            ASTOperand::Indirect(address) => write!(f, "(${:04X})", address),
            ASTOperand::IndirectIndexedX(address) => write!(f, "(${:02X},X)", address),
            ASTOperand::IndirectIndexedY(address) => write!(f, "(${:02X}),Y", address),
            ASTOperand::Immediate(value) => write!(f, "#${:02X}", value),
            ASTOperand::Implied => Ok(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ASTInstructionNode {
    /// Mnemonic of the instruction
    pub mnemonic: ASTMnemonic,
    /// Combined addressing mode and operand
    pub operand: ASTOperand,
}

impl fmt::Display for ASTInstructionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.mnemonic.is_implied() {
            write!(f, "{}", self.mnemonic)
        } else {
            write!(f, "{} {}", self.mnemonic, self.operand)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Instruction(ASTInstructionNode),
    Label(String),
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Instruction(instruction) => write!(f, "{}", instruction),
            ASTNode::Label(label) => write!(f, "{}:", label),
        }
    }
}

// pub struct ASTNode {
//     pub node: ASTNodeType,
//     // pub line_number: usize,
// }
