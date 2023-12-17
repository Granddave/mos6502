use self::opcode::OPCODE_MAPPING;
use self::symbol_resolver::{SymbolTable, SymbolType};
use crate::ast::{ASTAddressingMode, ASTInstructionNode, ASTNode, ASTOperand, AST};

/// Mapping from ASTInstruction to opcode.
mod opcode;

/// Resolves symbols in the AST.
mod symbol_resolver;

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

    /// Resolve labels to absolute and relative addresses. This is done by looking up the label in
    /// the symbol table and replacing the label with the address of the label.
    #[tracing::instrument]
    fn resolve_labels_to_addr(&mut self, ast: &mut AST) {
        let mut current_addr = 0;

        ast.iter_mut().for_each(|node| {
            if let ASTNode::Instruction(ins) = node {
                // The current address is pointing to the address of the next instruction.
                // The relative offset is calculated from the address of the following
                // instruction due to the fact that the CPU has already incremented the
                // program counter past the current instruction.
                current_addr += ins.size();
                if let ASTOperand::Label(label) = &ins.operand {
                    let symbol = self
                        .symbol_table
                        .find_symbol(label)
                        .expect("Label not found");
                    if let SymbolType::Label(absolute_offset_in_program) = symbol.symbol {
                        match ins.ins.addr_mode {
                            ASTAddressingMode::Absolute => {
                                let address = (absolute_offset_in_program as u16)
                                    .checked_add(self.program_offset)
                                    .expect("Overflow error");
                                ins.operand = ASTOperand::Absolute(address);
                            }
                            ASTAddressingMode::Relative => {
                                let offset_addr = (absolute_offset_in_program as u16)
                                    .wrapping_sub(current_addr as u16)
                                    as i8;
                                ins.operand = ASTOperand::Relative(offset_addr);
                            }
                            _ => panic!("Invalid addressing mode for label"),
                        }
                    }
                }
            }
        })
    }

    #[tracing::instrument]
    fn resolve_constants_to_values(&mut self, ast: &mut AST) {
        ast.iter_mut().for_each(|node| {
            if let ASTNode::Instruction(ins) = node {
                if let ASTOperand::Constant(constant) = &ins.operand {
                    match self
                        .symbol_table
                        .find_symbol(constant)
                        .expect("Constant not found")
                        .symbol
                    {
                        SymbolType::ConstantByte(byte) => match ins.ins.addr_mode {
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
                                ins.ins.addr_mode = ASTAddressingMode::ZeroPage;
                            }
                            _ => panic!("Invalid addressing mode for constant byte: {:#?}", ins),
                        },
                        SymbolType::ConstantWord(word) => match ins.ins.addr_mode {
                            ASTAddressingMode::Constant => {
                                // Special case for the absolute addressing mode since we at the
                                // parsing stage don't know if the operand is a byte or word.
                                ins.operand = ASTOperand::Absolute(word);
                                ins.ins.addr_mode = ASTAddressingMode::Absolute;
                            }
                            _ => panic!("Invalid addressing mode for constant word: {:#?}", ins),
                        },
                        _ => panic!("Invalid symbol type for constant operand: {:#?}", ins),
                    }
                }
            }
        })
    }

    /// Pass 1 of the compiler.
    ///
    /// This pass resolves labels and constants and verifies that all symbols are valid.
    #[tracing::instrument]
    fn pass_1(&mut self, ast: &mut AST) {
        // The constant resolver needs to be run before the label resolver since the label
        // resolver depends on the constant resolver to have resolved all constants to their
        // values.
        symbol_resolver::resolve_constants(ast, &mut self.symbol_table);
        self.resolve_constants_to_values(ast);

        symbol_resolver::resolve_labels(ast, &mut self.symbol_table);
        self.resolve_labels_to_addr(ast);

        // Verify that all symbols are valid before proceeding to the next pass
        symbol_resolver::verify_symbols(ast, &mut self.symbol_table);
    }

    /// Compile a single instruction node from the AST to machine code.
    #[tracing::instrument]
    fn compile_instruction(&mut self, ins: &ASTInstructionNode) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();

        bytes.push(
            OPCODE_MAPPING
                .find_opcode(ins.ins)
                .unwrap_or_else(|| panic!("Invalid instruction for opcode: {:#?}", ins)),
        );

        bytes.extend(match ins.operand {
            ASTOperand::Immediate(value) => vec![value],
            ASTOperand::Absolute(address) => vec![address as u8, (address >> 8) as u8],
            ASTOperand::ZeroPage(address) => vec![address],
            ASTOperand::Relative(offset) => vec![offset as u8],
            ASTOperand::Label(_) => panic!("Label should have been resolved to a relative offset"),
            ASTOperand::Constant(_) => panic!("Constant should have been resolved to its value"),
            ASTOperand::Implied => vec![],
        });

        self.current_address += ins.size() as u16;

        bytes
    }

    /// Pass 2 of the compiler.
    ///
    /// This pass generates machine code from the AST. The AST is assumed to have been resolved
    /// and all labels and constants have been replaced with their respective addresses and
    /// values.
    #[tracing::instrument]
    fn pass_2(&mut self, ast: &mut AST) -> Vec<u8> {
        ast.iter()
            .filter_map(|node| match node {
                ASTNode::Instruction(ins) => Some(ins),
                _ => None,
            })
            .flat_map(|ins| self.compile_instruction(ins))
            .collect()
    }

    /// Compile the AST to machine code.
    ///
    /// The AST is compiled in two passes:
    /// 1. Resolve labels and constants
    /// 2. Generate machine code
    #[tracing::instrument]
    pub fn compile(&mut self, ast: AST) -> Vec<u8> {
        let mut ast = ast;
        self.pass_1(&mut ast);
        self.pass_2(&mut ast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTAddressingMode, ASTConstantNode, ASTMnemonic, ASTNode, ASTOperand};

    // Test that the compiler can compile single instructions
    #[test]
    fn test_compile_ast() {
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
            let bytes = compiler.compile(ast);
            assert_eq!(bytes, expected);
        }
    }

    #[test]
    fn test_compile_program() {
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
            let bytes = compiler.compile(ast);
            assert_eq!(bytes, expected);
        }
    }

    #[test]
    fn test_program_offset() {
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
            let bytes = compiler.compile(ast.clone());
            assert_eq!(bytes, expected);
        }
    }
}
