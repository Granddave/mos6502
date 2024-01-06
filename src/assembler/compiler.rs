use self::symbol_resolver::{SymbolTable, SymbolType};
use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{AddressingMode, Instruction, Node, Operand, AST},
};

use thiserror::Error;

/// Mapping from instruction definitions to opcodes.
pub mod opcode;

/// Resolves symbols in the AST.
mod symbol_resolver;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum CompilerError {
    #[error("Symbol not found: {0}")]
    SymbolNotFound(String),
    #[error("Symbol already defined: {0}")]
    SymbolAlreadyDefined(String),
    #[error("Symbol not defined: {0}")]
    UndefinedSymbol(String),
    #[error("Invalid addressing mode: {0}")]
    InvalidAddressingMode(Instruction),
    #[error("Invalid symbol type for constant operand: {0}")]
    InvalidSymbolTypeForConstantOperand(Instruction),
    #[error("Invalid opcode: {0}")]
    InvalidOpcode(Instruction),
}

/// Compiler for the 6502 CPU.
///
/// The compiler compiles the [AST] into machine code.
#[derive(Debug)]
pub struct Compiler {
    symbol_table: SymbolTable,
    program_offset: u16,
}

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler::new(0x0000)
    }
}

impl Compiler {
    #[tracing::instrument]
    pub fn new(program_offset: u16) -> Compiler {
        Compiler {
            symbol_table: SymbolTable::new(),
            program_offset,
        }
    }

    fn resolve_label_to_addr(
        &mut self,
        ins: &mut Instruction,
        current_addr: usize,
    ) -> Result<(), CompilerError> {
        if let Operand::Label(label_operand) = &ins.operand {
            let label_symbol = match self.symbol_table.find_symbol(label_operand) {
                Some(symbol) => symbol,
                None => return Err(CompilerError::SymbolNotFound(label_operand.clone())),
            };

            if let SymbolType::Label(absolute_offset_in_program) = label_symbol.symbol {
                match ins.addr_mode {
                    AddressingMode::Absolute => {
                        let address = (absolute_offset_in_program as u16)
                            .checked_add(self.program_offset)
                            .expect("Overflow error");
                        ins.operand = Operand::Absolute(address);
                    }
                    AddressingMode::Relative => {
                        let offset_addr = (absolute_offset_in_program as u16)
                            .wrapping_sub(current_addr as u16)
                            as i8;
                        ins.operand = Operand::Relative(offset_addr);
                    }
                    _ => {
                        return Err(CompilerError::InvalidAddressingMode(ins.clone()));
                    }
                }
            }
        }

        Ok(())
    }

    /// Resolve labels to absolute and relative addresses. This is done by looking up the label in
    /// the symbol table and replacing the label with the address of the label.
    #[tracing::instrument]
    fn resolve_labels_to_addr(&mut self, ast: &mut AST) -> Result<(), CompilerError> {
        let mut current_addr = 0;

        for ins in ast.iter_mut().filter_map(|node| node.get_instruction()) {
            // The current address is pointing to the address of the next instruction.
            // The relative offset is calculated from the address of the following
            // instruction due to the fact that the CPU has already incremented the
            // program counter past the current instruction.
            current_addr += ins.size();
            self.resolve_label_to_addr(ins, current_addr)?;
        }

        Ok(())
    }

    #[tracing::instrument]
    fn resolve_constants_to_values(&mut self, ast: &mut AST) -> Result<(), CompilerError> {
        for ins in ast.iter_mut().filter_map(|node| node.get_instruction()) {
            if let Operand::Constant(constant) = &ins.operand {
                let symbol = match self.symbol_table.find_symbol(constant) {
                    Some(symbol) => symbol,
                    None => {
                        return Err(CompilerError::SymbolNotFound(constant.clone()));
                    }
                };

                match symbol.symbol {
                    SymbolType::ConstantByte(byte) => match ins.addr_mode {
                        AddressingMode::Immediate => {
                            ins.operand = Operand::Immediate(byte);
                        }
                        AddressingMode::ZeroPageX
                        | AddressingMode::ZeroPageY
                        | AddressingMode::IndirectIndexedX
                        | AddressingMode::IndirectIndexedY => {
                            ins.operand = Operand::ZeroPage(byte);
                        }
                        AddressingMode::Constant => {
                            // Special case for the zeropage addressing mode since we at the
                            // parsing stage don't know if the operand is a byte or word.
                            ins.operand = Operand::ZeroPage(byte);
                            ins.addr_mode = AddressingMode::ZeroPage;
                        }
                        _ => {
                            return Err(CompilerError::InvalidAddressingMode(ins.clone()));
                        }
                    },
                    SymbolType::ConstantWord(word) => match ins.addr_mode {
                        AddressingMode::Constant => {
                            // Special case for the absolute addressing mode since we at the
                            // parsing stage don't know if the operand is a byte or word.
                            ins.operand = Operand::Absolute(word);
                            ins.addr_mode = AddressingMode::Absolute;
                        }
                        _ => {
                            return Err(CompilerError::InvalidAddressingMode(ins.clone()));
                        }
                    },
                    _ => {
                        return Err(CompilerError::InvalidSymbolTypeForConstantOperand(
                            ins.clone(),
                        ))
                    }
                }
            }
        }

        Ok(())
    }

    /// Pass 1 of the compiler.
    ///
    /// This pass resolves labels and constants and verifies that all symbols are valid.
    #[tracing::instrument]
    fn pass_1(&mut self, ast: &mut AST) -> Result<(), CompilerError> {
        // The constant resolver needs to be run before the label resolver since the label
        // resolver depends on the constant resolver to have resolved all constants to their
        // values.
        symbol_resolver::resolve_constants(ast, &mut self.symbol_table);
        self.resolve_constants_to_values(ast)?;

        symbol_resolver::resolve_labels(ast, &mut self.symbol_table);
        self.resolve_labels_to_addr(ast)?;

        // Verify that all symbols are valid before proceeding to the next pass
        symbol_resolver::verify_symbols(ast, &mut self.symbol_table)?;

        Ok(())
    }

    /// Compile a single instruction node from the AST to machine code.
    #[tracing::instrument]
    pub fn instruction_to_bytes(ins: &Instruction) -> Result<Vec<u8>, CompilerError> {
        let mut bytes = vec![];

        bytes.push(
            match OPCODE_MAPPING.find_opcode((ins.mnemonic, ins.addr_mode)) {
                Some(bytes) => bytes,
                None => return Err(CompilerError::InvalidOpcode(ins.clone())),
            },
        );

        bytes.extend(match ins.operand {
            Operand::Immediate(value) => vec![value],
            Operand::Absolute(address) => vec![address as u8, (address >> 8) as u8],
            Operand::ZeroPage(address) => vec![address],
            Operand::Relative(offset) => vec![offset as u8],
            Operand::Implied => vec![],
            Operand::Label(_) => panic!("Label should have been resolved to a relative offset"),
            Operand::Constant(_) => panic!("Constant should have been resolved to its value"),
        });

        Ok(bytes)
    }

    /// Pass 2 of the compiler.
    ///
    /// This pass generates machine code from the AST. The AST is assumed to have been resolved
    /// and all labels and constants have been replaced with their respective addresses and
    /// values.
    #[tracing::instrument]
    fn pass_2(&mut self, ast: &mut AST) -> Result<Vec<u8>, CompilerError> {
        ast.iter()
            .filter_map(|node| match node {
                Node::Instruction(ins) => Some(ins),
                _ => None,
            })
            .try_fold(Vec::new(), |mut acc, ins| {
                let bytes = Compiler::instruction_to_bytes(ins)?;
                acc.extend(bytes);
                Ok(acc)
            })
    }

    /// Compile the AST to machine code.
    ///
    /// The AST is compiled in two passes:
    /// 1. Resolve labels and constants
    /// 2. Generate machine code
    #[tracing::instrument]
    pub fn compile(&mut self, ast: AST) -> Result<Vec<u8>, CompilerError> {
        let mut ast = ast;
        self.pass_1(&mut ast)?;
        let bytes = self.pass_2(&mut ast)?;

        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AddressingMode, Constant, Mnemonic, Node, Operand};

    use pretty_assertions::assert_eq;

    // Test that the compiler can compile single instructions
    #[test]
    fn test_compile_ast() -> Result<(), CompilerError> {
        // TODO: Test more instructions
        let tests = vec![
            (
                vec![Node::new_instruction(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0x01),
                )],
                vec![0xA9, 0x01],
            ),
            (
                vec![Node::new_instruction(
                    Mnemonic::LDA,
                    AddressingMode::Absolute,
                    Operand::Absolute(0x0200),
                )],
                vec![0xAD, 0x00, 0x02],
            ),
            (
                vec![Node::new_instruction(
                    Mnemonic::LDA,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(0x0200),
                )],
                vec![0xBD, 0x00, 0x02],
            ),
            (
                vec![
                    Node::new_instruction(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    ),
                    Node::new_instruction(
                        Mnemonic::LDA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    ),
                    Node::new_instruction(
                        Mnemonic::LDA,
                        AddressingMode::AbsoluteX,
                        Operand::Absolute(0x0200),
                    ),
                ],
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
            ),
        ];

        for (ast, expected) in tests {
            let mut compiler = Compiler::default();
            let bytes = compiler.compile(ast)?;
            assert_eq!(bytes, expected);
        }

        Ok(())
    }

    #[test]
    fn test_compile_program() -> Result<(), CompilerError> {
        let tests = vec![
            // test for relative branch instructions that verifies that the compiler can correctly
            // calculate the relative offset. Both forward and backward branches are tested.
            (
                vec![
                    Node::new_instruction(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x08),
                    ),
                    Node::Label("loop".to_string()),
                    Node::new_instruction(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    ),
                    Node::new_instruction(
                        Mnemonic::JMP,
                        AddressingMode::Absolute,
                        Operand::Label("end".to_string()),
                    ),
                    Node::new_instruction(
                        Mnemonic::STA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    ),
                    Node::new_instruction(
                        Mnemonic::BNE,
                        AddressingMode::Relative,
                        Operand::Label("loop".to_string()),
                    ),
                    Node::Label("end".to_string()),
                    Node::new_instruction(Mnemonic::BRK, AddressingMode::Implied, Operand::Implied),
                ],
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
            ),
            (
                vec![
                    Node::Constant(Constant::new_word("sysRandom".to_string(), 0xd010)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDY,
                        AddressingMode::Constant,
                        Operand::Constant("sysRandom".to_string()),
                    )),
                    Node::Constant(Constant::new_byte("a_dozen".to_string(), 0x0c)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Constant("a_dozen".to_string()),
                    )),
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Constant,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
                vec![
                    /* LDY */ 0xAC, 0x10, 0xD0, /* LDX */ 0xA2, 0x0C,
                    /* LDA */ 0xA5, 0x02,
                ],
            ),
        ];

        for (ast, expected) in tests {
            let mut compiler = Compiler::default();
            let bytes = compiler.compile(ast)?;
            assert_eq!(bytes, expected);
        }

        Ok(())
    }

    #[test]
    fn test_program_offset() -> Result<(), CompilerError> {
        let ast = vec![
            Node::new_instruction(
                Mnemonic::LDX,
                AddressingMode::Immediate,
                Operand::Immediate(0x08),
            ),
            Node::Label("loop".to_string()),
            Node::new_instruction(
                Mnemonic::LDA,
                AddressingMode::Immediate,
                Operand::Immediate(0x01),
            ),
            Node::new_instruction(
                Mnemonic::JMP,
                AddressingMode::Absolute,
                Operand::Label("end".to_string()),
            ),
            Node::new_instruction(
                Mnemonic::STA,
                AddressingMode::Absolute,
                Operand::Absolute(0x0200),
            ),
            Node::new_instruction(
                Mnemonic::BNE,
                AddressingMode::Relative,
                Operand::Label("loop".to_string()),
            ),
            Node::Label("end".to_string()),
            Node::new_instruction(Mnemonic::BRK, AddressingMode::Implied, Operand::Implied),
        ];

        let tests = vec![
            (
                0x0000,
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
            ),
            (
                0x0600,
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x06, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
            ),
            (
                0x8000,
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x80, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
            ),
        ];

        for (program_offset, expected) in tests {
            let mut compiler = Compiler::new(program_offset);
            let bytes = compiler.compile(ast.clone())?;
            assert_eq!(bytes, expected);
        }

        Ok(())
    }

    #[test]
    fn test_compile_errors() {
        let tests = vec![
            // TODO
        ];

        for (ast, expected) in tests {
            let mut compiler = Compiler::default();
            let output = compiler.compile(ast);
            assert_eq!(output, Err(expected));
        }
    }
}
