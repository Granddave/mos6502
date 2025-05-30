use std::fmt;

/// Commands to the assembler that are not instructions.
#[derive(Debug, PartialEq, Clone)]
pub enum Directive {
    /// Defines where in the memory code from now on should be placed.
    ///
    /// E.g. `.org $8000` in the assembly code. Instructs the code generator that code from now on
    /// should be placed at address $8000.
    ///
    /// The .org directive affects
    ///   - where the code from now on should be placed in the memory
    ///   - relative addressing of labels
    Origin(u16),
    /// Byte directive
    ///
    /// Defines a byte in the memory.
    /// E.g. `.byte $FF` in the assembly code. Instructs the code generator to place a byte with
    /// value $FF in the memory.
    Byte(u8),
    /// Word directive
    ///
    /// Defines a word in the memory.
    /// E.g. `.word $FFFF` in the assembly code. Instructs the code generator to place a word with
    /// value $FFFF in the memory.
    Word(u16),
    /// Ascii directive
    ///
    /// Defines a string in the memory.
    /// E.g. `.ascii "Hello"` in the assembly code. Instructs the code generator to place the ASCII
    /// string "Hello" in the memory.
    ///
    /// Can either be constructed from
    /// - `.ascii` For a non-null-terminated string
    /// - `.asciz` For a null-terminated string
    Ascii(String),
}

impl Directive {
    /// Returns the size of the directive in bytes.
    pub fn size(&self) -> usize {
        match self {
            Directive::Origin(_) => 2,      // .org takes 2 bytes (address)
            Directive::Byte(_) => 1,        // .byte takes 1 byte
            Directive::Word(_) => 2,        // .word takes 2 bytes
            Directive::Ascii(s) => s.len(), // .ascii takes the length of the string
        }
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Directive::Origin(address) => write!(f, ".org ${:04x}", address),
            Directive::Byte(byte) => write!(f, ".byte ${:02x}", byte),
            Directive::Word(word) => write!(f, ".word ${:04x}", word),
            Directive::Ascii(string) => write!(f, ".ascii \"{}\"", string),
        }
    }
}
