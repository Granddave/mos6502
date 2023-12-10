// Allow during initial development
#![allow(dead_code)]

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

/// Transforms 6502 assembly code to machine code.
///
/// The steps are:
/// 1. **Lexing** - converting a string into tokens
/// 2. **Parsing** - converting tokens into an AST
/// 3. **Compiling** - converting an AST into machine code in multiple passes
///     - Pass 1: Symbol resolution - resolving labels
///     - Pass 2: Code generation - generating machine code
pub mod assembler;

/// 6502 CPU emulator
pub mod emulator;

/// Hexdump utility
pub mod hexdump;
