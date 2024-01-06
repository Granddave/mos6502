/// An operand of an [`instruction`][super::Instruction].
///
/// This represents the actual data that is used by the instruction.
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Operand {
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    Relative(i8),
    /// `Label` is only used during parsing of the source code.
    /// A reference to a [`Node::Label`][crate::ast::Node#variant.Label] node.
    Label(String),
    /// `Constant` is only used during parsing of the source code.
    /// A reference to [`Node::Constant`][crate::ast::Node#variant.Constant] node.
    Constant(String),
    /// `Implied` covers both
    /// [`AddressingMode::Accumulator`][super::addressing_mode::AddressingMode#variant.Accumulator]
    /// and [`AddressingMode::Implied`][super::addressing_mode::AddressingMode#variant.Implied]
    Implied,
}
