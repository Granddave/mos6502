use std::fmt;

/// Mnemonic of an instruction.
///
/// This represents the operation that is performed by the instruction.
#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, strum_macros::EnumString)]
pub enum Mnemonic {
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

impl Mnemonic {
    pub fn is_jumping_instruction(&self) -> bool {
        matches!(self, Mnemonic::JMP | Mnemonic::JSR)
    }

    pub fn is_branching_instruction(&self) -> bool {
        matches!(
            self,
            Mnemonic::BCC
                | Mnemonic::BCS
                | Mnemonic::BEQ
                | Mnemonic::BMI
                | Mnemonic::BNE
                | Mnemonic::BPL
                | Mnemonic::BVC
                | Mnemonic::BVS
        )
    }

    pub fn has_implied_addressing_mode(&self) -> bool {
        matches!(
            self,
            Mnemonic::BRK
                | Mnemonic::CLC
                | Mnemonic::CLD
                | Mnemonic::CLI
                | Mnemonic::CLV
                | Mnemonic::DEX
                | Mnemonic::DEY
                | Mnemonic::INX
                | Mnemonic::INY
                | Mnemonic::NOP
                | Mnemonic::PHA
                | Mnemonic::PHP
                | Mnemonic::PLA
                | Mnemonic::PLP
                | Mnemonic::RTI
                | Mnemonic::RTS
                | Mnemonic::SEC
                | Mnemonic::SED
                | Mnemonic::SEI
                | Mnemonic::TAX
                | Mnemonic::TAY
                | Mnemonic::TSX
                | Mnemonic::TXA
                | Mnemonic::TXS
                | Mnemonic::TYA
        )
    }

    pub fn has_accumulator_addressing_mode(&self) -> bool {
        matches!(
            self,
            Mnemonic::ASL | Mnemonic::LSR | Mnemonic::ROL | Mnemonic::ROR
        )
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Why did it have spaces in front?
        write!(f, "{:?}", self)
    }
}

/// Addressing mode of an instruction.
///
/// This represents the way the instruction uses the operand.
#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub enum AddressingMode {
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
    /// [`Operand::Label`][self::Operand#variant.Label] is resolved to relative offsets
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

    /// Special addressing mode used in parsing: [`Operand::Constant`][self::Operand#variant.Constant]
    Constant,
}

/// An operand of an [`instruction`][self::Instruction].
///
/// This represents the actual data that is used by the instruction.
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Operand {
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
    /// `Implied` covers both [`AddressingMode::Accumulator`][self::AddressingMode#variant.Accumulator] and
    /// [`AddressingMode::Implied`][self::AddressingMode#variant.Implied]
    Implied,
}

/// A CPU instruction with an an optional operand and the addressing mode which tells the CPU how
/// to interpret the operand.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub addr_mode: AddressingMode,
    pub operand: Operand,
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic, addr_mode: AddressingMode, operand: Operand) -> Instruction {
        Instruction {
            mnemonic,
            addr_mode,
            operand,
        }
    }

    /// Size of instruction opcode + operand in bytes
    pub fn size(&self) -> usize {
        match self.operand {
            Operand::Immediate(_) => 2,
            Operand::Absolute(_) => 3,
            Operand::ZeroPage(_) => 2,
            Operand::Relative(_) => 2,
            Operand::Label(_) | Operand::Constant(_) => {
                // Labels are during compilation resolved to relative and absolute addresses
                // Constants are during compilation resolved to values which are either bytes or words
                match self.addr_mode {
                    AddressingMode::Absolute => 3,
                    AddressingMode::Relative => 2,
                    AddressingMode::Immediate => 2,
                    _ => panic!("Cannot calculata size for: {:#?}", self),
                }
            }
            Operand::Implied => 1,
        }
    }
}

impl fmt::Display for Instruction {
    // NOTE: This might not be correct for all addressing modes
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.addr_mode == AddressingMode::Implied {
            write!(f, "{}", self.mnemonic)
        } else {
            match &self.operand {
                Operand::Immediate(value) => match self.addr_mode {
                    AddressingMode::Immediate => {
                        write!(f, "{} #${:02X}", self.mnemonic, value)
                    }
                    AddressingMode::ZeroPage => {
                        write!(f, "{} ${:02X}", self.mnemonic, value)
                    }
                    _ => write!(f, "{} #${:02X}", self.mnemonic, value),
                },
                Operand::Absolute(address) => match self.addr_mode {
                    AddressingMode::Absolute => {
                        write!(f, "{} ${:04X}", self.mnemonic, address)
                    }
                    AddressingMode::AbsoluteX => {
                        write!(f, "{} ${:04X},X", self.mnemonic, address)
                    }
                    AddressingMode::AbsoluteY => {
                        write!(f, "{} ${:04X},Y", self.mnemonic, address)
                    }
                    AddressingMode::Indirect => {
                        write!(f, "{} (${:04X})", self.mnemonic, address)
                    }
                    _ => panic!("Invalid addressing mode for absolute address"),
                },
                Operand::ZeroPage(address) => match self.addr_mode {
                    AddressingMode::ZeroPage => {
                        write!(f, "{} ${:02X}", self.mnemonic, address)
                    }
                    AddressingMode::ZeroPageX => {
                        write!(f, "{} ${:02X},X", self.mnemonic, address)
                    }
                    AddressingMode::ZeroPageY => {
                        write!(f, "{} ${:02X},Y", self.mnemonic, address)
                    }
                    AddressingMode::IndirectIndexedX => {
                        write!(f, "{} (${:02X},X)", self.mnemonic, address)
                    }
                    AddressingMode::IndirectIndexedY => {
                        write!(f, "{} (${:02X}),Y", self.mnemonic, address)
                    }
                    _ => panic!("Invalid addressing mode for zero page address"),
                },
                Operand::Relative(offset) => write!(f, "{} ${:02X}", self.mnemonic, offset),
                Operand::Label(label) => write!(f, "{} {}", self.mnemonic, label),
                Operand::Implied => write!(f, "{}", self.mnemonic),
                Operand::Constant(constant) => match self.addr_mode {
                    AddressingMode::Absolute => write!(f, "{} {}", self.mnemonic, constant),
                    AddressingMode::ZeroPage => write!(f, "{} {}", self.mnemonic, constant),
                    AddressingMode::ZeroPageX => {
                        write!(f, "{} {},X", self.mnemonic, constant)
                    }
                    AddressingMode::ZeroPageY => {
                        write!(f, "{} {},Y", self.mnemonic, constant)
                    }
                    AddressingMode::AbsoluteX => {
                        write!(f, "{} {},X", self.mnemonic, constant)
                    }
                    AddressingMode::AbsoluteY => {
                        write!(f, "{} {},Y", self.mnemonic, constant)
                    }
                    AddressingMode::Indirect => {
                        write!(f, "{} ({})", self.mnemonic, constant)
                    }
                    AddressingMode::IndirectIndexedX => {
                        write!(f, "{} ({},X)", self.mnemonic, constant)
                    }
                    AddressingMode::IndirectIndexedY => {
                        write!(f, "{} ({}),Y", self.mnemonic, constant)
                    }
                    AddressingMode::Immediate => {
                        write!(f, "{} #{}", self.mnemonic, constant)
                    }
                    _ => panic!("Invalid addressing mode for constant"),
                },
            }
        }
    }
}

/// A constant value.
///
/// This is only used during parsing of the source code.
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
    Instruction(Instruction),
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
        mnemonic: Mnemonic,
        addr_mode: AddressingMode,
        operand: Operand,
    ) -> ASTNode {
        ASTNode::Instruction(Instruction::new(mnemonic, addr_mode, operand))
    }

    pub fn get_instruction(&mut self) -> Option<&mut Instruction> {
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
