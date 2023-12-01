/// Lexes code into tokens
mod lexer;

/// Parses code into an AST
mod parser;

/// Generates code from an AST
mod codegen;

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    Eof,
    Identifier,
    LiteralNumber,
    Hex,
    Label,
    Comma,
    ParenLeft,
    ParenRight,
}

#[derive(Debug, PartialEq, Clone)]
struct Token {
    token_type: TokenType,
    literal: String,
}

// enum ASTNode {
//     Instruction(Instruction, AddressingMode, Option<Operand>),
//     Label(String),
// }
