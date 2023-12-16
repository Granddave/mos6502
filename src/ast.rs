use std::fmt;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, strum_macros::EnumString)]
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
    pub fn is_jump(&self) -> bool {
        matches!(self, ASTMnemonic::JMP | ASTMnemonic::JSR)
    }

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

    pub fn is_accumulator(&self) -> bool {
        matches!(
            self,
            ASTMnemonic::ASL | ASTMnemonic::LSR | ASTMnemonic::ROL | ASTMnemonic::ROR
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
    // Special used for parsing
    Constant,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum ASTOperand {
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    Relative(i8),
    Label(String),
    Constant(String),
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

/// An instruction with an operand.
#[derive(Debug, PartialEq, Clone)]
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
        match self.operand {
            ASTOperand::Immediate(_) => 2,
            ASTOperand::Absolute(_) => 3,
            ASTOperand::ZeroPage(_) => 2,
            ASTOperand::Relative(_) => 2,
            ASTOperand::Label(_) | ASTOperand::Constant(_) => {
                // Labels are during compilation resolved to relative and absolute addresses
                // Constants are during compilation resolved to values which are either bytes or words
                match self.ins.addr_mode {
                    ASTAddressingMode::Absolute => 3,
                    ASTAddressingMode::Relative => 2,
                    ASTAddressingMode::Immediate => 2,
                    _ => panic!("Invalid addressing mode for label or constant: {:?}", self),
                }
            }
            ASTOperand::Implied => 1,
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
                    _ => panic!("Invalid addressing mode for absolute address"),
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
                    _ => panic!("Invalid addressing mode for zero page address"),
                },
                ASTOperand::Relative(offset) => write!(f, "{} ${:02X}", self.ins.mnemonic, offset),
                ASTOperand::Label(label) => write!(f, "{} {}", self.ins.mnemonic, label),
                ASTOperand::Implied => write!(f, "{}", self.ins.mnemonic),
                ASTOperand::Constant(constant) => match self.ins.addr_mode {
                    ASTAddressingMode::Absolute => write!(f, "{} {}", self.ins.mnemonic, constant),
                    ASTAddressingMode::ZeroPage => write!(f, "{} {}", self.ins.mnemonic, constant),
                    ASTAddressingMode::ZeroPageX => {
                        write!(f, "{} {},X", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::ZeroPageY => {
                        write!(f, "{} {},Y", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::AbsoluteX => {
                        write!(f, "{} {},X", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::AbsoluteY => {
                        write!(f, "{} {},Y", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::Indirect => {
                        write!(f, "{} ({})", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::IndirectIndexedX => {
                        write!(f, "{} ({},X)", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::IndirectIndexedY => {
                        write!(f, "{} ({}),Y", self.ins.mnemonic, constant)
                    }
                    ASTAddressingMode::Immediate => {
                        write!(f, "{} #{}", self.ins.mnemonic, constant)
                    }
                    _ => panic!("Invalid addressing mode for constant"),
                },
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTConstantValue {
    Byte(u8),
    Word(u16),
}

impl fmt::Display for ASTConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTConstantValue::Byte(value) => write!(f, "${:02x}", value),
            ASTConstantValue::Word(value) => write!(f, "${:04x}", value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ASTConstantNode {
    pub identifier: String,
    pub value: ASTConstantValue,
}

impl ASTConstantNode {
    pub fn new_byte(identifier: String, byte: u8) -> ASTConstantNode {
        ASTConstantNode {
            identifier,
            value: ASTConstantValue::Byte(byte),
        }
    }

    pub fn new_word(identifier: String, word: u16) -> ASTConstantNode {
        ASTConstantNode {
            identifier,
            value: ASTConstantValue::Word(word),
        }
    }
}

impl fmt::Display for ASTConstantNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Make sure it's padded
        write!(f, "define {} {}", self.identifier, self.value)
    }
}

/// AST Node is either an instruction (including operand) or a label.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Instruction(ASTInstructionNode),
    Label(String),
    Constant(ASTConstantNode),
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
            ASTNode::Constant(constant) => write!(f, "  {}", constant),
        }
    }
}

pub type AST = Vec<ASTNode>;
