use thiserror::Error;

use crate::{
    assembler::codegen::opcode::OPCODE_MAPPING,
    ast::{Directive, Instruction, Node, Operand, AST},
};

/// Mapping from instruction definitions to opcodes.
pub mod opcode;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum CodeGenError {
    #[error("Invalid opcode: {0}")]
    InvalidOpcode(Instruction),
    #[error("Program too large")]
    ProgramOverflow,
    #[error(".org directives not specified in ascending order, address: {0}")]
    OrgDirectiveNotInAscendingOrder(u16),
}

fn verify_org_directives(ast: &AST) -> Result<(), CodeGenError> {
    // The address must be within the range 0x0000-0xffff does not need to be verified
    // since the address is stored in a u16.

    // Warn about org directives that are not specified in ascending order
    let mut prev_org_addr = 0;
    for node in ast.iter() {
        if let Node::Directive(Directive::Origin(org_addr)) = node {
            if *org_addr < prev_org_addr {
                return Err(CodeGenError::OrgDirectiveNotInAscendingOrder(*org_addr));
            }
            prev_org_addr = *org_addr;
        }
    }

    Ok(())
}

/// Compile a single instruction to machine code.
#[tracing::instrument]
pub fn instruction_to_bytes(ins: &Instruction) -> Result<Vec<u8>, CodeGenError> {
    let mut bytes = vec![];

    bytes.push(
        match OPCODE_MAPPING.find_opcode((ins.mnemonic, ins.addr_mode)) {
            Some(bytes) => bytes,
            None => return Err(CodeGenError::InvalidOpcode(ins.clone())),
        },
    );

    bytes.extend(match ins.operand {
        Operand::Immediate(value) => vec![value],
        Operand::Absolute(address) => vec![address as u8, (address >> 8) as u8],
        Operand::ZeroPage(address) => vec![address],
        Operand::Relative(offset) => vec![offset as u8],
        Operand::Implied => vec![],
        // TODO: Return compiler crash
        Operand::Label(_) => panic!("Label should have been resolved to a relative offset"),
        Operand::Constant(_) => panic!("Constant should have been resolved to its value"),
    });

    Ok(bytes)
}

/// Generate machine code from the AST.
///
/// The AST is assumed to have been resolved and all labels and constants used by different
/// instructions have been replaced with their respective addresses and values.
#[tracing::instrument]
fn ast_to_bytes(ast: &mut AST) -> Result<Vec<u8>, CodeGenError> {
    let mut bytes = vec![];
    for node in ast.iter() {
        match node {
            Node::Instruction(ins) => {
                let ins_bytes = instruction_to_bytes(ins)?;
                bytes.extend(ins_bytes);
            }
            Node::Directive(directive) => match directive {
                Directive::Origin(org_addr) => {
                    // The .org directive should be possible to specify the address and the
                    // compiler should then insert padding bytes to fill the gap between the
                    // current address and the address specified in the .org directive.
                    // The current implementation assumes that the .org directives are
                    // specified in ascending order.
                    //
                    // Here we insert padding bytes to fill the gap between the current
                    // address and the address specified in the .org directive.

                    // TODO: Warn about overlapping org directives, i.e. the address of the
                    // current org directive does not overlap with the block of code generated
                    // by the previous org directive.

                    bytes.resize(*org_addr as usize, 0x00);
                }
                Directive::Byte(byte) => bytes.push(*byte),
                Directive::Word(word) => bytes.extend_from_slice(&word.to_le_bytes()),
            },
            _ => (),
        }
    }

    Ok(bytes)
}

/// Compile the AST to machine code.
#[tracing::instrument]
pub fn generate(ast: AST) -> Result<Vec<u8>, CodeGenError> {
    verify_org_directives(&ast)?;

    let mut ast = ast;
    let bytes = ast_to_bytes(&mut ast)?;
    if bytes.len() > 0xffff {
        return Err(CodeGenError::ProgramOverflow);
    }

    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AddressingMode, Constant, Mnemonic, Node, Operand};

    use pretty_assertions::assert_eq;

    // Test that the compiler can compile single instructions
    #[test]
    fn test_compile_ast() -> Result<(), CodeGenError> {
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
            let bytes = generate(ast)?;
            assert_eq!(bytes, expected);
        }

        Ok(())
    }

    // Disable during refactoring of symbol resolver
    // TODO: Change the input to be an resolved AST *or* move the test to the test directory.
    #[ignore]
    #[test]
    fn test_compile_program() -> Result<(), CodeGenError> {
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
            (
                vec![
                    Node::Directive(Directive::Byte(0x01)),
                    Node::Directive(Directive::Byte(0x02)),
                    Node::Directive(Directive::Word(0x0403)),
                ],
                vec![0x01, 0x02, 0x03, 0x04],
            ),
        ];

        for (ast, expected) in tests {
            let bytes = generate(ast)?;
            assert_eq!(bytes, expected);
        }

        Ok(())
    }

    // TODO: Add test for .org directive

    #[test]
    fn test_compile_errors() {
        let tests = vec![
            // Test ascending order of .org directives
            (
                vec![
                    Node::Directive(Directive::Origin(0x0200)),
                    Node::Directive(Directive::Origin(0x0100)),
                ],
                CodeGenError::OrgDirectiveNotInAscendingOrder(0x0100),
            ),
        ];

        for (ast, expected) in tests {
            let output = generate(ast);
            assert_eq!(output, Err(expected));
        }
    }
}
