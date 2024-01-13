use anyhow::{Context, Result};
use clap::Args;

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

#[derive(Debug, thiserror::Error)]
pub enum AssemblerError {
    #[error("Parser error: {0}")]
    Parse(#[from] parser::ParseError),
    #[error("Compiler error: {0}")]
    Compile(#[from] compiler::CompilerError),
}

#[derive(Args, Debug)]
pub struct AssemblyArgs {
    input: String,
    #[clap(short, value_name = "FILENAME", default_value = "a.bin")]
    output: String,
}

/// Utility function for generating machine code from an assembly program.
#[tracing::instrument]
pub fn compile_code(input: &str, _program_start: u16) -> Result<Vec<u8>, AssemblerError> {
    // TODO: Remove _program_start
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer)?;
    let ast = parser.parse_program()?;

    let mut compiler = compiler::Compiler::new();
    let program = compiler.compile(ast)?;

    Ok(program)
}

#[tracing::instrument]
pub fn assemble(args: &AssemblyArgs) -> Result<()> {
    const PROGRAM_START: u16 = 0x8000;
    let input_source =
        std::fs::read_to_string(&args.input).with_context(|| "Unable to read file")?;
    let bytes = compile_code(&input_source, PROGRAM_START).with_context(|| "Compilation failed")?;
    std::fs::write(&args.output, bytes).with_context(|| "Unable to write file")?;
    Ok(())
}
