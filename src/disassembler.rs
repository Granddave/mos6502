use crate::ast::{ASTAddressingMode, ASTInstruction, ASTInstructionNode, ASTNode, ASTOperand};

pub mod listing;

#[derive(Debug, Default)]
pub struct Disassembler {
    input: Vec<u8>,
    curr_ix: usize,
}

impl Disassembler {
    #[tracing::instrument]
    pub fn new(input: Vec<u8>) -> Self {
        Self { input, curr_ix: 0 }
    }

    #[tracing::instrument]
    fn decode_operand(&mut self, addressing_mode: ASTAddressingMode) -> ASTOperand {
        let ix = self.curr_ix;

        macro_rules! byte {
            () => {{
                self.input[ix]
            }};
        }
        macro_rules! word {
            () => {{
                ((self.input[ix + 1] as u16) << 8) | self.input[ix] as u16
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
    fn decode_opcode(&mut self, opcode: u8) -> ASTInstruction {
        crate::assembler::compiler::opcode::OPCODE_MAPPING
            .find_instruction(opcode)
            .unwrap_or_else(|| panic!("Invalid opcode: '{:#04x}'", opcode))
    }

    #[tracing::instrument]
    fn decode_instruction(&mut self) -> ASTNode {
        let ins = self.decode_opcode(self.input[self.curr_ix]);
        self.curr_ix += 1; // Step over opcode

        let operand = self.decode_operand(ins.addr_mode);
        let instruction = ASTInstructionNode { ins, operand };
        self.curr_ix += instruction.size() - 1; // Step over operand

        ASTNode::Instruction(instruction)
    }

    #[tracing::instrument]
    pub fn disassemble_code(&mut self) -> Vec<ASTNode> {
        let mut ast = vec![];

        while self.curr_ix < self.input.len() {
            ast.push(self.decode_instruction());
        }

        ast
    }
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
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(0x01),
                )],
            ),
            (
                vec![0xAD, 0x00, 0x02],
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xBD, 0x00, 0x02],
                vec![ASTNode::new_instruction(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::AbsoluteX,
                    ASTOperand::Absolute(0x0200),
                )],
            ),
            (
                vec![0xA9, 0x01, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02],
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
            ),
            (
                vec![
                    /* LDX */ 0xA2, 0x08, /* LDA */ 0xA9, 0x01, /* JMP */ 0x4C,
                    0x0C, 0x00, /* STA */ 0x8D, 0x00, 0x02, /* BNE */ 0xD0, 0xF6,
                    /* BRK */ 0x00,
                ],
                vec![
                    ASTNode::new_instruction(
                        ASTMnemonic::LDX,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x08),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Immediate(0x01),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::JMP,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x000C),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::STA,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Absolute(0x0200),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::BNE,
                        ASTAddressingMode::Relative,
                        ASTOperand::Relative(-10),
                    ),
                    ASTNode::new_instruction(
                        ASTMnemonic::BRK,
                        ASTAddressingMode::Implied,
                        ASTOperand::Implied,
                    ),
                ],
            ),
        ];

        for (input, expected) in tests {
            let mut disassembler = Disassembler::new(input);
            let actual = disassembler.disassemble_code();
            assert_eq!(actual, expected);
        }
    }
}
