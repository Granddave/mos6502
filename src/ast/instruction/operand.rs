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
