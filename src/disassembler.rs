use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Args;

use crate::ast::{AddressingMode, Directive, Instruction, Mnemonic, Node, Operand};

pub mod listing;

#[derive(Args, Debug)]
pub struct DisassemblyArgs {
    #[clap(help = "Input file to disassemble")]
    input: PathBuf,
}

#[tracing::instrument]
pub fn disassemble(args: &DisassemblyArgs) -> Result<()> {
    let bytes = std::fs::read(&args.input).with_context(|| "Unable to read file")?;
    let ast = disassemble_code(&bytes)?;
    println!("{}", listing::generate(0x0000, ast));
    Ok(())
}

#[tracing::instrument]
fn decode_operand(input: &[u8], ix: usize, addressing_mode: AddressingMode) -> Option<Operand> {
    macro_rules! byte {
        () => {{
            input[ix]
        }};
    }
    macro_rules! word {
        () => {{
            ((input[ix + 1] as u16) << 8) | input[ix] as u16
        }};
    }

    match addressing_mode {
        AddressingMode::Absolute => Some(Operand::Absolute(word!())),
        AddressingMode::ZeroPage => Some(Operand::ZeroPage(byte!())),
        AddressingMode::ZeroPageX => Some(Operand::ZeroPage(byte!())),
        AddressingMode::ZeroPageY => Some(Operand::ZeroPage(byte!())),
        AddressingMode::AbsoluteX => Some(Operand::Absolute(word!())),
        AddressingMode::AbsoluteY => Some(Operand::Absolute(word!())),
        AddressingMode::Relative => Some(Operand::Relative(byte!() as i8)),
        AddressingMode::Indirect => Some(Operand::Absolute(word!())),
        AddressingMode::IndirectIndexedX => Some(Operand::ZeroPage(byte!())),
        AddressingMode::IndirectIndexedY => Some(Operand::ZeroPage(byte!())),
        AddressingMode::Immediate => Some(Operand::Immediate(byte!())),
        AddressingMode::Accumulator => Some(Operand::Implied),
        AddressingMode::Implied => Some(Operand::Implied),
        _ => None,
    }
}

#[tracing::instrument]
fn decode_opcode(opcode: u8) -> Option<(Mnemonic, AddressingMode)> {
    crate::assembler::codegen::opcode::OPCODE_MAPPING.find_instruction(opcode)
    // .unwrap_or_else(|| panic!("Invalid opcode: '{:#04x}'", opcode))
}

#[tracing::instrument]
fn decode_instruction(input: &[u8], ix: usize) -> Result<Instruction> {
    // TODO: If decoding fails, treat it as '.byte' instead and continue.
    // TODO: Return Result<Disassemly> where
    // enum Disassemly {
    //     Instruction(Instruction),
    //     Byte(u8),
    // }
    let (mnemonic, addr_mode) = decode_opcode(input[ix]).with_context(|| {
        format!(
            "Invalid opcode: '{:#04x}' at address: {:#06x}",
            input[ix], ix
        )
    })?;
    let operand = decode_operand(input, ix + 1, addr_mode).with_context(|| {
        format!(
            "Invalid operand for address mode: '{:?}' at address: {:#06x}",
            addr_mode, ix
        )
    })?;
    Ok(Instruction {
        mnemonic,
        addr_mode,
        operand,
    })
}

/// Disassembles a slice of bytes into a vector of AST nodes.
///
/// This is a best-effort disassembly, meaning that it will try to decode as many instructions as
/// possible, and if it encounters an invalid opcode, it will treat that byte as a directive
/// (e.g., `.byte`).
pub fn disassemble_code(input: &[u8]) -> Result<Vec<Node>> {
    let mut code = vec![];
    let mut ix = 0;

    while ix < input.len() {
        match decode_instruction(input, ix) {
            Ok(ins) => {
                ix += ins.size();
                code.push(Node::Instruction(ins));
            }
            Err(_) => {
                code.push(Node::Directive(Directive::Byte(input[ix])));
                ix += 1;
            }
        }
    }

    Ok(code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Mnemonic;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_disassemble_code() -> Result<()> {
        let tests = vec![
            (
                vec![0xA9, 0x01],
                vec![Node::Instruction(Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0x01),
                ))],
            ),
            (
                vec![0xAD, 0x00, 0x02],
                vec![Node::Instruction(Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Absolute,
                    Operand::Absolute(0x0200),
                ))],
            ),
            (
                vec![0xBD, 0x00, 0x02],
                vec![Node::Instruction(Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(0x0200),
                ))],
            ),
            (
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
                vec![
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::AbsoluteX,
                        Operand::Absolute(0x0200),
                    )),
                ],
            ),
            (
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
                vec![
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x08),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::JMP,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x000C),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::STA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::BNE,
                        AddressingMode::Relative,
                        Operand::Relative(-10),
                    )),
                    Node::Instruction(Instruction::new(
                        Mnemonic::BRK,
                        AddressingMode::Implied,
                        Operand::Implied,
                    )),
                ],
            ),
            (
                vec![0x02, 0xA2, 0x08],
                vec![
                    Node::Directive(Directive::Byte(0x02)), // The first value that does not correspond to a valid opcode
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x08),
                    )),
                ],
            ),
        ];

        for (input, expected) in tests {
            let actual = disassemble_code(input.as_slice())?;
            assert_eq!(actual, expected);
        }
        Ok(())
    }
}
