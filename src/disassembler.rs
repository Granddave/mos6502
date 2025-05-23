use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Args;

use crate::ast::{AddressingMode, Instruction, Mnemonic, Node, Operand};

pub mod listing;

#[derive(Args, Debug)]
pub struct DisassemblyArgs {
    #[clap(help = "Input file to disassemble")]
    input: PathBuf,
}

#[tracing::instrument]
pub fn disassemble(args: &DisassemblyArgs) -> Result<()> {
    let bytes = std::fs::read(&args.input).with_context(|| "Unable to read file")?;
    let instructions = disassemble_code(&bytes)?;
    let ast = instructions.into_iter().map(Node::Instruction).collect();
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

pub fn disassemble_code(input: &[u8]) -> Result<Vec<Instruction>> {
    let mut code = vec![];
    let mut ix = 0;

    while ix < input.len() {
        let node = decode_instruction(input, ix)?;
        code.push(node.clone());
        ix += node.size();
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
                vec![Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0x01),
                )],
            ),
            (
                vec![0xAD, 0x00, 0x02],
                vec![Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Absolute,
                    Operand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xBD, 0x00, 0x02],
                vec![Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
                vec![
                    Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    ),
                    Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    ),
                    Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::AbsoluteX,
                        Operand::Absolute(0x0200),
                    ),
                ],
            ),
            (
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
                vec![
                    Instruction::new(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x08),
                    ),
                    Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Immediate,
                        Operand::Immediate(0x01),
                    ),
                    Instruction::new(
                        Mnemonic::JMP,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x000C),
                    ),
                    Instruction::new(
                        Mnemonic::STA,
                        AddressingMode::Absolute,
                        Operand::Absolute(0x0200),
                    ),
                    Instruction::new(
                        Mnemonic::BNE,
                        AddressingMode::Relative,
                        Operand::Relative(-10),
                    ),
                    Instruction::new(Mnemonic::BRK, AddressingMode::Implied, Operand::Implied),
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
