// Error handling:
// - Use anyhow for user facing errors
// - Use thiserror for internal errors
//
// Error from inside the library should be converted to anyhow::Error before returning to the user.

/// AST (Abstract Syntax Tree) node types.
///
/// The AST is a tree representation of the source code.
///
/// For example, `LDA #$C8` is represented as:
///
/// ```
/// # use mos6502::ast::{Instruction, Mnemonic, AddressingMode, Operand};
/// let instruction = Instruction::new(
///     Mnemonic::LDA,
///     AddressingMode::Immediate,
///     Operand::Immediate(0xC8),
/// );
/// ```
pub mod ast;

/// Transforms 6502 assembly code to machine code.
///
/// The steps are:
/// 1. **[Lexing][assembler::lexer::Lexer]** - converting a string into tokens
/// 2. **[Parsing][assembler::parser::Parser]** - converting tokens into an AST
/// 3. **[Symbol resolution][assembler::symbols]** - resolving symbols like constants and labels in the AST
/// 4. **[Code generator][assembler::codegen]** - converting an AST into machine code
pub mod assembler;

/// Transforms machine code to its AST (Abstract Syntax Tree) representation.
///
/// Usage:
/// ```
/// use mos6502::{
///     ast::{AddressingMode, Instruction, Mnemonic, Operand},
///     disassembler::disassemble_code,
/// };
///
/// assert_eq!(
///     disassemble_code(&[0xA9, 0xC8]).unwrap(),
///     vec![Instruction::new(
///         Mnemonic::LDA,
///         AddressingMode::Immediate,
///         Operand::Immediate(0xC8)
///     )]
/// );
/// ```
pub mod disassembler;

/// 6502 CPU emulator
pub mod emulator;

/// Hexdump utility
pub mod hexdump;
