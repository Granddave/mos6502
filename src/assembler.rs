/// Lexes code into tokens.
///
/// Converts a string into tokens. For example, the string `LDA #$10` would be
/// converted into the following tokens:
///
/// ```text
/// [
///     Token { token: TokenType::Identifier, literal: "LDA", line_number: 1, },
///     Token { token: TokenType::LiteralNumber, literal: "#", line_number: 1, },
///     Token { token: TokenType::Hex, literal: "C8", line_number: 1, },
///     Token { token: TokenType::Eof, literal: "".to_string(), line_number: 2, },
/// ]
pub mod lexer;

/// AST (Abstract Syntax Tree) node types.
///
/// The AST is a tree representation of the source code.
///
/// For example, `LDA #$C8` is represented as:
///
/// ```text
/// ASTInstructionNode::new(
///     ASTMnemonic::LDA,
///     ASTAddressingMode::Immediate,
///     ASTOperand::Immediate(0xC8),
/// ),
pub mod ast;

/// Parses tokens into an AST.
pub mod parser;

/// Generates machine code from an AST.
pub mod compiler;
