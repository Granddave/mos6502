use std::fmt;

/// Mnemonic of an instruction.
///
/// This represents the operation that is performed by the instruction.
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
    pub fn is_jumping_instruction(&self) -> bool {
        matches!(self, ASTMnemonic::JMP | ASTMnemonic::JSR)
    }

    pub fn is_branching_instruction(&self) -> bool {
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

    pub fn has_implied_addressing_mode(&self) -> bool {
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

    pub fn has_accumulator_addressing_mode(&self) -> bool {
        matches!(
            self,
            ASTMnemonic::ASL | ASTMnemonic::LSR | ASTMnemonic::ROL | ASTMnemonic::ROR
        )
    }
}

impl fmt::Display for ASTMnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Why did it have spaces in front?
        write!(f, "{:?}", self)
    }
}

/// Addressing mode of an instruction.
///
/// This represents the way the instruction uses the operand.
#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub enum ASTAddressingMode {
    // Addressing modes
    /// `a`
    Absolute,
    /// `zp`
    ZeroPage,
    /// `zp,x`
    ZeroPageX,
    /// `zp,y`
    ZeroPageY,
    /// `a,x`
    AbsoluteX,
    /// `a,y`
    AbsoluteY,
    /// `r` for branch instructions.
    /// [`ASTOperand::Label`][self::ASTOperand#variant.Label] is resolved to relative offsets
    Relative,
    /// `(a)`
    Indirect,
    /// `(zp,x)`
    IndirectIndexedX,
    /// `(zp),y`
    IndirectIndexedY,

    /// `#v`
    Immediate,

    Accumulator,
    Implied,

    /// Special addressing mode used in parsing: [`ASTOperand::Constant`][self::ASTOperand#variant.Constant]
    Constant,
}

/// An operand of an [`instruction`][self::ASTInstruction].
///
/// This represents the actual data that is used by the instruction.
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum ASTOperand {
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    Relative(i8),
    /// `Label` is only used during parsing of the source code.
    /// A reference to a [`ASTNode::Label`][crate::ast::ASTNode#variant.Label] node.
    Label(String),
    /// `Constant` is only used during parsing of the source code.
    /// A reference to [`ASTNode::Constant`][crate::ast::ASTNode#variant.Constant] node.
    Constant(String),
    /// `Implied` covers both [`ASTAddressingMode::Accumulator`][self::ASTAddressingMode#variant.Accumulator] and
    /// [`ASTAddressingMode::Implied`][self::ASTAddressingMode#variant.Implied]
    Implied,
}

/// An instruction without an operand.
#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct ASTInstruction {
    /// Mnemonic of the instruction
    pub mnemonic: ASTMnemonic,
    /// Addressing mode of the instruction
    pub addr_mode: ASTAddressingMode,
}

impl fmt::Display for ASTInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?}", self.mnemonic, self.addr_mode)
    }
}

/// An instruction with an operand.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ASTInstructionNode {
    /// The type of instruction
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

    /// Size of instruction opcode + operand in bytes
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
                    _ => panic!("Cannot calculata size for: {:#?}", self),
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

/// A constant value.
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

/// A constant definition.
#[derive(Debug, PartialEq, Clone)]
pub struct ASTConstantNode {
    /// The name of the constant (e.g. `max_items`)
    pub identifier: String,
    /// The value of the constant
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

/// A single node in the AST, i.e. a single line in the source code.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    /// A CPU instruction
    Instruction(ASTInstructionNode),
    /// A label to mark a location in the code
    ///
    /// Labels are resolved to absolute addresses during compilation.
    /// E.g. `init:`
    Label(String),
    /// A constant definition
    ///
    /// E.g. `define max_items $FF`
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

    pub fn get_instruction(&mut self) -> Option<&mut ASTInstructionNode> {
        match self {
            ASTNode::Instruction(instruction) => Some(instruction),
            _ => None,
        }
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

/// An AST (Abstract Syntax Tree) is a collection of AST nodes.
///
/// The AST is the result of parsing the source code.
pub type AST = Vec<ASTNode>;
