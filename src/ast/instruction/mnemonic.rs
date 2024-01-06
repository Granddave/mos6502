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
