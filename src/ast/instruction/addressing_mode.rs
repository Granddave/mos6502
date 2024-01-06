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
