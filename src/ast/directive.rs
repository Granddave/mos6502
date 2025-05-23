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
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Directive::Origin(address) => write!(f, ".org ${:04x}", address),
        }
    }
}
