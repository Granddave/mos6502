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

/// Parses tokens into an AST.
pub mod parser;

/// Generates machine code from an AST.
pub mod compiler;

/// Utility functions for generating machine code.
///
/// TODO: Pass in a target address.
#[tracing::instrument]
pub fn compile_code(input: &str) -> Result<Vec<u8>, anyhow::Error> {
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let ast = parser.parse_program()?;
    let mut compiler = compiler::Compiler::new(0x0600);

    Ok(compiler.compile(ast))
}
