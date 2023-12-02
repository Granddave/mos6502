/// Lexes code into tokens
pub mod lexer;

/// AST node types
pub mod ast;

/// Parses code into an AST
pub mod parser;

/// Generates machine code from an AST
mod codegen;
