use self::symbol_resolver::{SymbolTable, SymbolType};
use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{ASTAddressingMode, ASTInstructionNode, ASTNode, ASTOperand, AST},
};

use thiserror::Error;

/// Mapping from ASTInstruction to opcode.
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
    InvalidAddressingMode(ASTInstructionNode),
    #[error("Invalid symbol type for constant operand: {0}")]
    InvalidSymbolTypeForConstantOperand(ASTInstructionNode),
    #[error("Invalid opcode: {0}")]
    InvalidOpcode(ASTInstructionNode),
}

/// Compiler for the 6502 CPU.
///
/// The compiler compiles the [AST] into machine code.
#[derive(Debug)]
pub struct Compiler {
    symbol_table: SymbolTable,
    current_address: u16,
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
            current_address: 0,
            program_offset,
        }
    }

    fn resolve_label_to_addr(
        &mut self,
        ins_node: &mut ASTInstructionNode,
        current_addr: usize,
    ) -> Result<(), CompilerError> {
        if let ASTOperand::Label(label_operand) = &ins_node.operand {
            let label_symbol = match self.symbol_table.find_symbol(label_operand) {
                Some(symbol) => symbol,
                None => return Err(CompilerError::SymbolNotFound(label_operand.clone())),
            };

            if let SymbolType::Label(absolute_offset_in_program) = label_symbol.symbol {
                match ins_node.addr_mode {
                    ASTAddressingMode::Absolute => {
                        let address = (absolute_offset_in_program as u16)
                            .checked_add(self.program_offset)
                            .expect("Overflow error");
                        ins_node.operand = ASTOperand::Absolute(address);
                    }
                    ASTAddressingMode::Relative => {
                        let offset_addr = (absolute_offset_in_program as u16)
                            .wrapping_sub(current_addr as u16)
                            as i8;
                        ins_node.operand = ASTOperand::Relative(offset_addr);
                    }
                    _ => {
                        return Err(CompilerError::InvalidAddressingMode(ins_node.clone()));
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

        for ins_node in ast.iter_mut().filter_map(|node| node.get_instruction()) {
            // The current address is pointing to the address of the next instruction.
            // The relative offset is calculated from the address of the following
            // instruction due to the fact that the CPU has already incremented the
            // program counter past the current instruction.
            current_addr += ins_node.size();
            self.resolve_label_to_addr(ins_node, current_addr)?;
        }

        Ok(())
    }

    #[tracing::instrument]
    fn resolve_constants_to_values(&mut self, ast: &mut AST) -> Result<(), CompilerError> {
        for ins in ast.iter_mut().filter_map(|node| node.get_instruction()) {
            if let ASTOperand::Constant(constant) = &ins.operand {
                let symbol = match self.symbol_table.find_symbol(constant) {
                    Some(symbol) => symbol,
                    None => {
                        return Err(CompilerError::SymbolNotFound(constant.clone()));
                    }
                };

                match symbol.symbol {
                    SymbolType::ConstantByte(byte) => match ins.addr_mode {
                        ASTAddressingMode::Immediate => {
                            ins.operand = ASTOperand::Immediate(byte);
                        }
                        ASTAddressingMode::ZeroPageX
                        | ASTAddressingMode::ZeroPageY
                        | ASTAddressingMode::IndirectIndexedX
                        | ASTAddressingMode::IndirectIndexedY => {
                            ins.operand = ASTOperand::ZeroPage(byte);
                        }
                        ASTAddressingMode::Constant => {
                            // Special case for the zeropage addressing mode since we at the
                            // parsing stage don't know if the operand is a byte or word.
                            ins.operand = ASTOperand::ZeroPage(byte);
                            ins.addr_mode = ASTAddressingMode::ZeroPage;
                        }
                        _ => {
                            return Err(CompilerError::InvalidAddressingMode(ins.clone()));
                        }
                    },
                    SymbolType::ConstantWord(word) => match ins.addr_mode {
                        ASTAddressingMode::Constant => {
                            // Special case for the absolute addressing mode since we at the
                            // parsing stage don't know if the operand is a byte or word.
                            ins.operand = ASTOperand::Absolute(word);
                            ins.addr_mode = ASTAddressingMode::Absolute;
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
    pub fn instruction_to_bytes(ins: &ASTInstructionNode) -> Result<Vec<u8>, CompilerError> {
        let mut bytes = vec![];

        bytes.push(
            match OPCODE_MAPPING.find_opcode((ins.mnemonic, ins.addr_mode)) {
                Some(bytes) => bytes,
                None => return Err(CompilerError::InvalidOpcode(ins.clone())),
            },
        );

        bytes.extend(match ins.operand {
            ASTOperand::Immediate(value) => vec![value],
            ASTOperand::Absolute(address) => vec![address as u8, (address >> 8) as u8],
            ASTOperand::ZeroPage(address) => vec![address],
            ASTOperand::Relative(offset) => vec![offset as u8],
            ASTOperand::Implied => vec![],
            ASTOperand::Label(_) => panic!("Label should have been resolved to a relative offset"),
            ASTOperand::Constant(_) => panic!("Constant should have been resolved to its value"),
        });

        Ok(bytes)
    }

    /// Compile a instruction to machine code and increment address.
    #[tracing::instrument]
    fn compile_instruction(&mut self, ins: &ASTInstructionNode) -> Result<Vec<u8>, CompilerError> {
        let bytes: Vec<u8> = Compiler::instruction_to_bytes(ins)?;
        self.current_address += ins.size() as u16;
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
                ASTNode::Instruction(ins) => Some(ins),
                _ => None,
            })
            .try_fold(Vec::new(), |mut acc, ins| {
                let bytes = self.compile_instruction(ins)?;
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
    use crate::ast::{ASTAddressingMode, ASTConstantNode, ASTMnemonic, ASTNode, ASTOperand};

    use pretty_assertions::assert_eq;

    // Test that the compiler can compile single instructions
    #[test]
    fn test_compile_ast() -> Result<(), CompilerError> {
        // TODO: Test more instructions
        let tests = vec![
            (
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(0x01),
                )],
                vec![0xA9, 0x01],
            ),
            (
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(0x0200),
                )],
                vec![0xAD, 0x00, 0x02],
            ),
            (
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::AbsoluteX,
                    ASTOperand::Absolute(0x0200),
                )],
                vec![0xBD, 0x00, 0x02],
            ),
            (
                vec![
                    ASTNode::new_instruction(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x01),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x0200),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::AbsoluteX,
                        ASTOperand::Absolute(0x0200),
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
                    ASTNode::new_instruction(
                        ASTMnemonic::LDX,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x08),
                    ),
                    ASTNode::Label("loop".to_string()),
                    ASTNode::new_instruction(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x01),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::JMP,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Label("end".to_string()),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::STA,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x0200),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::BNE,
                        ASTAddressingMode::Relative,
                        ASTOperand::Label("loop".to_string()),
                    ),
                    ASTNode::Label("end".to_string()),
                    ASTNode::new_instruction(
                        ASTMnemonic::BRK,
                        ASTAddressingMode::Implied,
                        ASTOperand::Implied,
                    ),
                ],
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
            ),
            (
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::Constant,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                    ASTNode::Constant(ASTConstantNode::new_byte("a_dozen".to_string(), 0x0c)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDX,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Constant("a_dozen".to_string()),
                    )),
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Constant,
                        ASTOperand::Constant("zpage".to_string()),
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
            ASTNode::new_instruction(
                ASTMnemonic::LDX,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x08),
            ),
            ASTNode::Label("loop".to_string()),
            ASTNode::new_instruction(
                ASTMnemonic::LDA,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x01),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::JMP,
                ASTAddressingMode::Absolute,
                ASTOperand::Label("end".to_string()),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::STA,
                ASTAddressingMode::Absolute,
                ASTOperand::Absolute(0x0200),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::BNE,
                ASTAddressingMode::Relative,
                ASTOperand::Label("loop".to_string()),
            ),
            ASTNode::Label("end".to_string()),
            ASTNode::new_instruction(
                ASTMnemonic::BRK,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            ),
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
