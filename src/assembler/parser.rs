use std::str::FromStr;

use crate::assembler::{
    ast::ASTNode,
    lexer::{Lexer, Token, TokenType},
};

use super::ast::{ASTAddressingMode, ASTInstructionNode, ASTMnemonic, ASTOperand};

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    // Errors?
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        // Feed the lexer so its tokens are ready to be consumed

        let mut s = Self {
            lexer,
            current_token: Token::new(),
            peek_token: Token::new(),
        };

        s.next_token();
        s.next_token();

        s
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone(); // TODO: Can we avoid clone here?
        self.peek_token = self.lexer.next_token().expect("Next token");
    }

    fn current_token_is(&self, token_type: TokenType) -> bool {
        self.current_token.token == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token == token_type
    }

    fn parse_label(&mut self) -> String {
        if self.current_token_is(TokenType::Identifier) && self.peek_token_is(TokenType::Colon) {
            let label = self.current_token.literal.clone();
            self.next_token(); // Consume the colon
            label
        } else {
            panic!(
                "Expected identifier followed by colon, got {:?}, {:?}",
                self.current_token.token, self.peek_token.token
            );
        }
    }

    fn parse_mnemonic(&mut self) -> ASTMnemonic {
        match ASTMnemonic::from_str(self.current_token.literal.as_str()) {
            Ok(mnemonic) => mnemonic,
            Err(err) => panic!("Invalid mnemonic: {}", err),
        }
    }

    fn try_parse_hex_byte(&mut self) -> Option<u8> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u8::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    fn try_parse_hex_word(&mut self) -> Option<u16> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u16::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    fn parse_addressing_mode_and_operand(
        &mut self,
        mnemonic: &ASTMnemonic,
    ) -> (ASTAddressingMode, ASTOperand) {
        if mnemonic.is_implied() {
            return (ASTAddressingMode::Implied, ASTOperand::Implied);
        }

        self.next_token();

        match self.current_token.token {
            TokenType::LiteralNumber => {
                self.next_token();
                match self.current_token.token {
                    TokenType::Hex => {
                        if let Some(byte) = self.try_parse_hex_byte() {
                            (ASTAddressingMode::Immediate, ASTOperand::Immediate(byte))
                        } else {
                            panic!("Invalid hex byte");
                        }
                    }
                    _ => panic!("Invalid literal number"),
                }
            }
            TokenType::Hex => {
                if let Some(byte) = self.try_parse_hex_byte() {
                    if self.peek_token_is(TokenType::Comma) {
                        self.next_token();
                        if !self.peek_token_is(TokenType::Identifier) {
                            panic!("Invalid ZeroPageX/Y operand")
                        }
                        self.next_token();
                        match self.current_token.literal.as_str() {
                            "X" => (ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(byte)),
                            "Y" => (ASTAddressingMode::ZeroPageY, ASTOperand::ZeroPage(byte)),
                            _ => panic!("Invalid ZeroPageX/Y operand"),
                        }
                    } else if mnemonic.is_branch() {
                        (
                            ASTAddressingMode::Relative,
                            ASTOperand::Relative(byte as i8),
                        )
                    } else {
                        (ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(byte))
                    }
                } else if let Some(word) = self.try_parse_hex_word() {
                    if self.peek_token_is(TokenType::Comma) {
                        self.next_token();
                        if !self.peek_token_is(TokenType::Identifier) {
                            panic!("Invalid hex operand");
                        }
                        self.next_token();
                        match self.current_token.literal.as_str() {
                            "X" => (ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(word)),
                            "Y" => (ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(word)),
                            _ => panic!("Invalid AbsoluteX/Y operand"),
                        }
                    } else {
                        (ASTAddressingMode::Absolute, ASTOperand::Absolute(word))
                    }
                } else {
                    panic!("Invalid hex operand");
                }
            }
            TokenType::ParenLeft => {
                // Indirect addressing
                self.next_token();

                if let Some(byte) = self.try_parse_hex_byte() {
                    if self.peek_token_is(TokenType::Comma) {
                        // Indirect indexed X
                        self.next_token(); // Consume the comma
                        if !self.peek_token_is(TokenType::Identifier) {
                            panic!("Invalid indirect indexed X operand");
                        }
                        self.next_token(); // Consume the identifier
                        let operand = match self.current_token.literal.as_str() {
                            "X" => (
                                ASTAddressingMode::IndirectIndexedX,
                                ASTOperand::ZeroPage(byte),
                            ),
                            _ => panic!("Invalid indirect indexed X operand"),
                        };
                        self.next_token(); // Consume the closing parenthesis
                        operand
                    } else if self.peek_token_is(TokenType::ParenRight) {
                        // Indirect indexed Y
                        self.next_token(); // Consume the closing parenthesis
                        if !self.peek_token_is(TokenType::Comma) {
                            panic!("Invalid indirect indexed Y operand");
                        }
                        self.next_token(); // Consume the comma
                        self.next_token(); // Consume the identifier
                        match self.current_token.literal.as_str() {
                            "Y" => (
                                ASTAddressingMode::IndirectIndexedY,
                                ASTOperand::ZeroPage(byte),
                            ),
                            _ => panic!("Invalid indirect indexed Y operand"),
                        }
                    } else {
                        panic!("Invalid indirect indexed X/Y operand");
                    }
                } else if let Some(word) = self.try_parse_hex_word() {
                    self.next_token();
                    (ASTAddressingMode::Indirect, ASTOperand::Absolute(word))
                } else {
                    panic!("Invalid indirect operand")
                }
            }
            TokenType::Identifier => {
                // Label
                (
                    ASTAddressingMode::Relative,
                    ASTOperand::Label(self.current_token.literal.clone()),
                )
            }
            _ => panic!("Invalid operand"),
        }
    }

    fn parse_instruction(&mut self) -> ASTInstructionNode {
        if self.current_token_is(TokenType::Identifier) {
            let mnemonic = self.parse_mnemonic();
            let (addressing_mode, operand) = self.parse_addressing_mode_and_operand(&mnemonic);

            ASTInstructionNode {
                mnemonic,
                addr_mode: addressing_mode,
                operand,
            }
        } else {
            panic!("Expected identifier, got {:?}", self.current_token.token);
        }
    }

    fn parse_node(&mut self) -> ASTNode {
        match &self.current_token.token {
            TokenType::Identifier => {
                if self.peek_token.token == TokenType::Colon {
                    ASTNode::Label(self.parse_label())
                } else {
                    ASTNode::Instruction(self.parse_instruction())
                }
            }
            _ => panic!(
                "parse_node: Unexpected token type: {:?}",
                self.current_token.token
            ),
        }
    }

    pub fn parse_program(&mut self) -> Vec<ASTNode> {
        let mut ast_nodes = Vec::new();
        'l: loop {
            if self.current_token_is(TokenType::Eof) {
                break 'l;
            }

            let ast_node = self.parse_node();
            ast_nodes.push(ast_node);
            self.next_token();
        }

        ast_nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assembler::lexer::Lexer;
    #[test]
    fn test_parse_label() {
        let tests = vec![("label:", "label".to_string())];

        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            assert_eq!(parser.parse_label(), expected);
        }
    }

    #[test]
    fn test_parse_mnemonic() {
        // A subset of the 6502 mnemonics
        let tests = vec![
            ("LDA", ASTMnemonic::LDA),
            ("LDX", ASTMnemonic::LDX),
            ("LDY", ASTMnemonic::LDY),
            ("LSR", ASTMnemonic::LSR),
            ("NOP", ASTMnemonic::NOP),
            ("ORA", ASTMnemonic::ORA),
            ("PHA", ASTMnemonic::PHA),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            assert_eq!(parser.parse_mnemonic(), expected);
        }
    }

    #[test]
    fn test_instruction() {
        let tests = vec![
            (
                "ADC $BEEF",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::ADC,
                    addr_mode: ASTAddressingMode::Absolute,
                    operand: ASTOperand::Absolute(0xBEEF),
                },
            ),
            (
                "ADC $C8",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::ADC,
                    addr_mode: ASTAddressingMode::ZeroPage,
                    operand: ASTOperand::ZeroPage(0xC8),
                },
            ),
            (
                "INC $C8,X",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::INC,
                    addr_mode: ASTAddressingMode::ZeroPageX,
                    operand: ASTOperand::ZeroPage(0xC8),
                },
            ),
            (
                "LDX $C8,Y",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::LDX,
                    addr_mode: ASTAddressingMode::ZeroPageY,
                    operand: ASTOperand::ZeroPage(0xC8),
                },
            ),
            (
                "CMP $BEEF,X",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::CMP,
                    addr_mode: ASTAddressingMode::AbsoluteX,
                    operand: ASTOperand::Absolute(0xBEEF),
                },
            ),
            (
                "EOR $BEEF,Y",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::EOR,
                    addr_mode: ASTAddressingMode::AbsoluteY,
                    operand: ASTOperand::Absolute(0xBEEF),
                },
            ),
            (
                "BEQ $03",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::BEQ,
                    addr_mode: ASTAddressingMode::Relative,
                    operand: ASTOperand::Relative(0x03),
                },
            ),
            (
                "JMP ($BEEF)",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::JMP,
                    addr_mode: ASTAddressingMode::Indirect,
                    operand: ASTOperand::Absolute(0xBEEF),
                },
            ),
            (
                "EOR ($C8,X)",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::EOR,
                    addr_mode: ASTAddressingMode::IndirectIndexedX,
                    operand: ASTOperand::ZeroPage(0xC8),
                },
            ),
            (
                "LDA ($C8),Y",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::LDA,
                    addr_mode: ASTAddressingMode::IndirectIndexedY,
                    operand: ASTOperand::ZeroPage(0xC8),
                },
            ),
            (
                "LDA #$C8",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::LDA,
                    addr_mode: ASTAddressingMode::Immediate,
                    operand: ASTOperand::Immediate(0xC8),
                },
            ),
            (
                "BRK",
                ASTInstructionNode {
                    mnemonic: ASTMnemonic::BRK,
                    addr_mode: ASTAddressingMode::Implied,
                    operand: ASTOperand::Implied,
                },
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            assert_eq!(parser.parse_instruction(), expected);
        }
    }

    #[test]
    fn test_parse_program() {
        let input = "  LDX #$00
  LDY #$00
firstloop:
  TXA
  STA $0200,Y
  PHA
  INX
  INY
  CPY #$10
  BNE firstloop
secondloop:
  PLA
  STA $0200,Y
  INY
  CPY #$20
  BNE secondloop";
        let expected = vec![
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::LDX,
                addr_mode: ASTAddressingMode::Immediate,
                operand: ASTOperand::Immediate(0x00),
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::LDY,
                addr_mode: ASTAddressingMode::Immediate,
                operand: ASTOperand::Immediate(0x00),
            }),
            ASTNode::Label("firstloop".to_string()),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::TXA,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::STA,
                addr_mode: ASTAddressingMode::AbsoluteY,
                operand: ASTOperand::Absolute(0x0200),
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::PHA,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::INX,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::INY,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::CPY,
                addr_mode: ASTAddressingMode::Immediate,
                operand: ASTOperand::Immediate(0x10),
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::BNE,
                addr_mode: ASTAddressingMode::Relative,
                operand: ASTOperand::Label("firstloop".to_string()),
            }),
            ASTNode::Label("secondloop".to_string()),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::PLA,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::STA,
                addr_mode: ASTAddressingMode::AbsoluteY,
                operand: ASTOperand::Absolute(0x0200),
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::INY,
                addr_mode: ASTAddressingMode::Implied,
                operand: ASTOperand::Implied,
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::CPY,
                addr_mode: ASTAddressingMode::Immediate,
                operand: ASTOperand::Immediate(0x20),
            }),
            ASTNode::Instruction(ASTInstructionNode {
                mnemonic: ASTMnemonic::BNE,
                addr_mode: ASTAddressingMode::Relative,
                operand: ASTOperand::Label("secondloop".to_string()),
            }),
        ];
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        assert_eq!(program, expected);
        assert_eq!(
            program
                .iter()
                .map(|node| node.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
            input
        );
    }
}
