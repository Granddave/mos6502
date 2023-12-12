use std::str::FromStr;

use crate::{
    assembler::lexer::{Lexer, Token, TokenType},
    ast::{
        ASTAddressingMode, ASTConstantNode, ASTInstructionNode, ASTMnemonic, ASTNode, ASTOperand,
    },
};

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

    fn try_parse_hex_u8(&mut self) -> Option<u8> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u8::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    fn try_parse_hex_u16(&mut self) -> Option<u16> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u16::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    fn parse_literal_number(&mut self) -> (ASTAddressingMode, ASTOperand) {
        self.next_token();
        match self.current_token.token {
            TokenType::Hex => {
                if let Some(byte) = self.try_parse_hex_u8() {
                    (ASTAddressingMode::Immediate, ASTOperand::Immediate(byte))
                } else {
                    panic!("Invalid hex byte");
                }
            }
            _ => panic!("Invalid literal number"),
        }
    }

    fn parse_hex_byte(
        &mut self,
        byte: u8,
        mnemonic: &ASTMnemonic,
    ) -> (ASTAddressingMode, ASTOperand) {
        if self.peek_token_is(TokenType::Comma) {
            // ZeroPageX/Y
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
    }

    fn parse_hex_word(&mut self, word: u16) -> (ASTAddressingMode, ASTOperand) {
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
    }

    fn parse_hex(&mut self, mnemonic: &ASTMnemonic) -> (ASTAddressingMode, ASTOperand) {
        if let Some(byte) = self.try_parse_hex_u8() {
            self.parse_hex_byte(byte, mnemonic)
        } else if let Some(word) = self.try_parse_hex_u16() {
            self.parse_hex_word(word)
        } else {
            panic!("Invalid hex operand");
        }
    }

    fn parse_indirect_indexed_x(&mut self, byte: u8) -> (ASTAddressingMode, ASTOperand) {
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
    }

    fn parse_indirect_indexed_y(&mut self, byte: u8) -> (ASTAddressingMode, ASTOperand) {
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
    }

    fn parse_indirect(&mut self) -> (ASTAddressingMode, ASTOperand) {
        self.next_token();

        if let Some(byte) = self.try_parse_hex_u8() {
            if self.peek_token_is(TokenType::Comma) {
                self.parse_indirect_indexed_x(byte)
            } else if self.peek_token_is(TokenType::ParenRight) {
                self.parse_indirect_indexed_y(byte)
            } else {
                panic!("Invalid indirect indexed X/Y operand");
            }
        } else if let Some(word) = self.try_parse_hex_u16() {
            self.next_token();
            (ASTAddressingMode::Indirect, ASTOperand::Absolute(word))
        } else {
            panic!("Invalid indirect operand")
        }
    }

    fn addressing_mode_for_label(&self, mnemonic: &ASTMnemonic) -> ASTAddressingMode {
        // TODO: Is this correct?
        if mnemonic.is_branch() {
            ASTAddressingMode::Relative
        } else {
            ASTAddressingMode::Absolute
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
            TokenType::LiteralNumber => self.parse_literal_number(),
            TokenType::Hex => self.parse_hex(mnemonic),
            TokenType::ParenLeft => self.parse_indirect(),
            TokenType::Identifier => (
                self.addressing_mode_for_label(mnemonic),
                ASTOperand::Label(self.current_token.literal.clone()),
            ),
            _ => panic!("Invalid operand"),
        }
    }

    fn parse_instruction(&mut self) -> ASTInstructionNode {
        if self.current_token_is(TokenType::Identifier) {
            let mnemonic = self.parse_mnemonic();
            let (addr_mode, operand) = self.parse_addressing_mode_and_operand(&mnemonic);

            ASTInstructionNode::new(mnemonic, addr_mode, operand)
        } else {
            panic!("Expected identifier, got {:?}", self.current_token.token);
        }
    }

    fn parse_constant(&mut self) -> ASTConstantNode {
        todo!()
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
            TokenType::Define => ASTNode::Constant(self.parse_constant()),
            _ => panic!(
                "parse_node: Unexpected token type: {:?}",
                self.current_token.token
            ),
        }
    }

    /// Parse the entire program into an AST
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
                ASTInstructionNode::new(
                    ASTMnemonic::ADC,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(0xBEEF),
                ),
            ),
            (
                "ADC $C8",
                ASTInstructionNode::new(
                    ASTMnemonic::ADC,
                    ASTAddressingMode::ZeroPage,
                    ASTOperand::ZeroPage(0xC8),
                ),
            ),
            (
                "INC $C8,X",
                ASTInstructionNode::new(
                    ASTMnemonic::INC,
                    ASTAddressingMode::ZeroPageX,
                    ASTOperand::ZeroPage(0xC8),
                ),
            ),
            (
                "LDX $C8,Y",
                ASTInstructionNode::new(
                    ASTMnemonic::LDX,
                    ASTAddressingMode::ZeroPageY,
                    ASTOperand::ZeroPage(0xC8),
                ),
            ),
            (
                "CMP $BEEF,X",
                ASTInstructionNode::new(
                    ASTMnemonic::CMP,
                    ASTAddressingMode::AbsoluteX,
                    ASTOperand::Absolute(0xBEEF),
                ),
            ),
            (
                "EOR $BEEF,Y",
                ASTInstructionNode::new(
                    ASTMnemonic::EOR,
                    ASTAddressingMode::AbsoluteY,
                    ASTOperand::Absolute(0xBEEF),
                ),
            ),
            (
                "BEQ $03",
                ASTInstructionNode::new(
                    ASTMnemonic::BEQ,
                    ASTAddressingMode::Relative,
                    ASTOperand::Relative(0x03),
                ),
            ),
            (
                "JMP ($BEEF)",
                ASTInstructionNode::new(
                    ASTMnemonic::JMP,
                    ASTAddressingMode::Indirect,
                    ASTOperand::Absolute(0xBEEF),
                ),
            ),
            (
                "EOR ($C8,X)",
                ASTInstructionNode::new(
                    ASTMnemonic::EOR,
                    ASTAddressingMode::IndirectIndexedX,
                    ASTOperand::ZeroPage(0xC8),
                ),
            ),
            (
                "LDA ($C8),Y",
                ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::IndirectIndexedY,
                    ASTOperand::ZeroPage(0xC8),
                ),
            ),
            (
                "LDA #$C8",
                ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(0xC8),
                ),
            ),
            (
                "BRK",
                ASTInstructionNode::new(
                    ASTMnemonic::BRK,
                    ASTAddressingMode::Implied,
                    ASTOperand::Implied,
                ),
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            assert_eq!(parser.parse_instruction(), expected);
        }
    }

    #[test]
    fn test_parse_constant() {
        let tests = vec![
            (
                "define zero $00",
                vec![ASTNode::Constant(ASTConstantNode::new_byte(
                    "zero".to_string(),
                    0x00,
                ))],
            ),
            (
                "define sysRandom $d010",
                vec![ASTNode::Constant(ASTConstantNode::new_word(
                    "sysRandom".to_string(),
                    0xd010,
                ))],
            ),
            (
                "define sysRandom $d010\nLDY sysRandom",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::Absolute,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                //
                "define sysRandom $d010\nLDY (sysRandom)",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::Indirect,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define sysRandom $d010\nLDY (sysRandom,X)",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::IndirectIndexedX,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define sysRandom $d010\nLDY (sysRandom),Y",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::IndirectIndexedY,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define a_dozen $0c\nLDX #a_dozen",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("a_dozen".to_string(), 0x0c)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDX,
                        ASTAddressingMode::Immediate,
                        ASTOperand::Constant("a_dozen".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nLDA zpage",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::ZeroPage,
                        ASTOperand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nLDA zpage,X",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::ZeroPageX,
                        ASTOperand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nSTA (zpage,X)",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::STA,
                        ASTAddressingMode::IndirectIndexedX,
                        ASTOperand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nSTA (zpage),Y",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::STA,
                        ASTAddressingMode::IndirectIndexedY,
                        ASTOperand::Constant("zpage".to_string()),
                    )),
                ],
            ),
        ];

        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            eprintln!("-----");
            eprintln!("input: \n\n{}\n", input);
            assert_eq!(parser.parse_program(), expected);
        }
    }

    #[test]
    fn test_parse_program() {
        let input = "
define zero $00

  LDX #zero
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
            ASTNode::Constant(ASTConstantNode::new_byte("zero".to_string(), 0x00)),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::LDX,
                ASTAddressingMode::Immediate,
                ASTOperand::Constant("zero".to_string()),
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::LDY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x00),
            )),
            ASTNode::Label("firstloop".to_string()),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::TXA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::STA,
                ASTAddressingMode::AbsoluteY,
                ASTOperand::Absolute(0x0200),
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::PHA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::INX,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::INY,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::CPY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x10),
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::BNE,
                ASTAddressingMode::Relative,
                ASTOperand::Label("firstloop".to_string()),
            )),
            ASTNode::Label("secondloop".to_string()),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::PLA,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::STA,
                ASTAddressingMode::AbsoluteY,
                ASTOperand::Absolute(0x0200),
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::INY,
                ASTAddressingMode::Implied,
                ASTOperand::Implied,
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::CPY,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x20),
            )),
            ASTNode::Instruction(ASTInstructionNode::new(
                ASTMnemonic::BNE,
                ASTAddressingMode::Relative,
                ASTOperand::Label("secondloop".to_string()),
            )),
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
