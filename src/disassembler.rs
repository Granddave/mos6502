use crate::ast::{ASTAddressingMode, ASTInstruction, ASTInstructionNode, ASTOperand};

pub mod listing;

#[tracing::instrument]
fn decode_operand(input: &[u8], curr_ix: usize, addressing_mode: ASTAddressingMode) -> ASTOperand {
    let ix = curr_ix;

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
        ASTAddressingMode::Absolute => ASTOperand::Absolute(word!()),
        ASTAddressingMode::ZeroPage => ASTOperand::ZeroPage(byte!()),
        ASTAddressingMode::ZeroPageX => ASTOperand::ZeroPage(byte!()),
        ASTAddressingMode::ZeroPageY => ASTOperand::ZeroPage(byte!()),
        ASTAddressingMode::AbsoluteX => ASTOperand::Absolute(word!()),
        ASTAddressingMode::AbsoluteY => ASTOperand::Absolute(word!()),
        ASTAddressingMode::Relative => ASTOperand::Relative(byte!() as i8),
        ASTAddressingMode::Indirect => ASTOperand::Absolute(word!()),
        ASTAddressingMode::IndirectIndexedX => ASTOperand::ZeroPage(byte!()),
        ASTAddressingMode::IndirectIndexedY => ASTOperand::ZeroPage(byte!()),
        ASTAddressingMode::Immediate => ASTOperand::Immediate(byte!()),
        ASTAddressingMode::Accumulator => ASTOperand::Implied,
        ASTAddressingMode::Implied => ASTOperand::Implied,
        _ => panic!("Invalid addressing mode: '{:#?}'", addressing_mode),
    }
}

#[tracing::instrument]
fn decode_opcode(opcode: u8) -> ASTInstruction {
    crate::assembler::compiler::opcode::OPCODE_MAPPING
        .find_instruction(opcode)
        .unwrap_or_else(|| panic!("Invalid opcode: '{:#04x}'", opcode))
}

#[tracing::instrument]
fn decode_instruction(input: &[u8], ix: usize) -> ASTInstructionNode {
    let ins = decode_opcode(input[ix]);
    let operand = decode_operand(input, ix + 1, ins.addr_mode);
    ASTInstructionNode { ins, operand }
}

pub fn disassemble_code(input: &[u8]) -> Vec<ASTInstructionNode> {
    let mut code = vec![];
    let mut curr_ix = 0;

    while curr_ix < input.len() {
        let node = decode_instruction(input, curr_ix);
        code.push(node.clone());
        curr_ix += node.size();
    }

    code
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ASTMnemonic;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_disassemble_code() {
        let tests = vec![
            (
                vec![0xA9, 0x01],
                vec![ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(0x01),
                )],
            ),
            (
                vec![0xAD, 0x00, 0x02],
                vec![ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xBD, 0x00, 0x02],
                vec![ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::AbsoluteX,
                    ASTOperand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
                vec![
                    ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x01),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x0200),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::AbsoluteX,
                        ASTOperand::Absolute(0x0200),
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
                    ASTInstructionNode::new(
                        ASTMnemonic::LDX,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x08),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x01),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::JMP,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x000C),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::STA,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x0200),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::BNE,
                        ASTAddressingMode::Relative,
                        ASTOperand::Relative(-10),
                    ),
                    ASTInstructionNode::new(
                        ASTMnemonic::BRK,
                        ASTAddressingMode::Implied,
                        ASTOperand::Implied,
                    ),
                ],
            ),
        ];

        for (input, expected) in tests {
            let actual = disassemble_code(input.as_slice());
            assert_eq!(actual, expected);
        }
    }
}
