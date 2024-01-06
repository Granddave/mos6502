pub mod addressing_mode;
pub mod mnemonic;
pub mod operand;

use std::fmt;

pub use addressing_mode::AddressingMode;
pub use mnemonic::Mnemonic;
pub use operand::Operand;

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
