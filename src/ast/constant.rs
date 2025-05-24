use std::fmt;

/// A constant value.
///
/// This is only used during parsing of the source code.
#[derive(Debug, PartialEq, Clone)]
pub enum ConstantValue {
    Byte(u8),
    Word(u16),
}

impl fmt::Display for ConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantValue::Byte(value) => write!(f, "${:02x}", value),
            ConstantValue::Word(value) => write!(f, "${:04x}", value),
        }
    }
}

/// A constant definition.
#[derive(Debug, PartialEq, Clone)]
pub struct Constant {
    /// The name of the constant (e.g. `max_items`)
    pub identifier: String,
    /// The value of the constant
    pub value: ConstantValue,
}

impl Constant {
    pub fn new_byte(identifier: String, byte: u8) -> Constant {
        Constant {
            identifier,
            value: ConstantValue::Byte(byte),
        }
    }

    pub fn new_word(identifier: String, word: u16) -> Constant {
        Constant {
            identifier,
            value: ConstantValue::Word(word),
        }
    }

    pub fn size(&self) -> usize {
        match self.value {
            ConstantValue::Byte(_) => 1,
            ConstantValue::Word(_) => 2,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Make sure it's padded
        write!(f, "define {} {}", self.identifier, self.value)
    }
}
