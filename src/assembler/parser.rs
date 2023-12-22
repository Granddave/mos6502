use std::{collections::VecDeque, str::FromStr};

use crate::{
    assembler::lexer::{token::Token, token::TokenType, Lexer},
    ast::{
        ASTAddressingMode, ASTConstantNode, ASTInstructionNode, ASTMnemonic, ASTNode, ASTOperand,
        AST,
    },
};

/// Allow the parser to peek `PEEK_BUFFER_SIZE` tokens in advance
const PEEK_BUFFER_SIZE: usize = 2;

/// A recursive descent parser for the 6502 assembly language.
///
/// It's using a [Lexer] to tokenize the input into a stream of tokens.
/// The parser then uses the tokens to build an [AST] (abstract syntax tree) which later can be used
/// to generate machine code.
#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    peek_tokens: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    #[tracing::instrument]
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        // Feed the lexer so its tokens are ready to be consumed

        let mut parser = Self {
            lexer,
            current_token: Token::default(),
            peek_tokens: VecDeque::with_capacity(PEEK_BUFFER_SIZE),
        };

        for _ in 0..PEEK_BUFFER_SIZE {
            parser.load_next_token();
        }

        parser.next_token(); // Load the first token

        parser
    }

    #[tracing::instrument]
    fn load_next_token(&mut self) {
        self.peek_tokens
            .push_back(self.lexer.next_token().expect("Failed to load next token"));
    }

    #[tracing::instrument]
    fn next_token(&mut self) {
        self.load_next_token();
        self.current_token = self.peek_tokens.pop_front().unwrap();
    }

    #[tracing::instrument]
    fn peek_token(&self, peek_ahead: usize) -> Token {
        self.peek_tokens
            .get(peek_ahead)
            .expect("Peeking past lookahead buffer")
            .to_owned()
    }

    #[tracing::instrument]
    fn current_token_is(&self, token_type: TokenType) -> bool {
        self.current_token.token == token_type
    }

    #[tracing::instrument]
    fn peek_token_is(&self, lookahead: usize, token_type: TokenType) -> bool {
        self.peek_tokens.get(lookahead).expect("peek").token == token_type
    }

    #[tracing::instrument]
    fn parse_label(&mut self) -> String {
        if self.current_token_is(TokenType::Identifier) && self.peek_token_is(0, TokenType::Colon) {
            let label = self.current_token.literal.clone();
            self.next_token(); // Consume the colon
            label
        } else {
            panic!(
                "Expected identifier followed by colon, got {:#?}",
                self.current_token.token
            );
        }
    }

    #[tracing::instrument]
    fn parse_mnemonic(&mut self) -> ASTMnemonic {
        match ASTMnemonic::from_str(self.current_token.literal.to_uppercase().as_str()) {
            Ok(mnemonic) => mnemonic,
            Err(err) => panic!("Invalid mnemonic: {}: {}", err, self.current_token.literal),
        }
    }

    #[tracing::instrument]
    fn try_parse_hex_u8(&mut self) -> Option<u8> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u8::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_hex_u16(&mut self) -> Option<u16> {
        let operand = self.current_token.literal.clone();
        let operand = operand.trim_start_matches('$');
        match u16::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_identifier(&mut self) -> Option<String> {
        if self.current_token_is(TokenType::Identifier) {
            Some(self.current_token.literal.clone())
        } else {
            None
        }
    }

    /// Parse a literal number, i.e. a hex byte, decimal byte or a constant
    /// The literal number is denoted by the following tokens:
    /// - Hex: #$xx
    /// - Decimal: #xx
    /// - Identifier: #constant
    #[tracing::instrument]
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
            TokenType::Decimal => {
                if let Ok(byte) = self.current_token.literal.parse::<u8>() {
                    (ASTAddressingMode::Immediate, ASTOperand::Immediate(byte))
                } else {
                    panic!("Invalid decimal byte");
                }
            }
            TokenType::Identifier => {
                if let Some(identifier) = self.try_parse_identifier() {
                    (
                        ASTAddressingMode::Immediate,
                        ASTOperand::Constant(identifier),
                    )
                } else {
                    panic!("Invalid identifier");
                }
            }
            _ => panic!(
                "Invalid literal number, got {:#?}",
                self.current_token.token
            ),
        }
    }

    // TODO: Rename to generic parse_byte
    #[tracing::instrument]
    fn parse_hex_byte(
        &mut self,
        byte: u8,
        mnemonic: &ASTMnemonic,
    ) -> (ASTAddressingMode, ASTOperand) {
        if self.peek_token_is(0, TokenType::Comma) {
            // ZeroPageX/Y
            self.next_token();
            if !self.peek_token_is(0, TokenType::Identifier) {
                panic!("Invalid ZeroPageX/Y operand")
            }
            self.next_token();
            match self.current_token.literal.to_uppercase().as_str() {
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

    // TODO: Rename to generic parse_word
    #[tracing::instrument]
    fn parse_hex_word(&mut self, word: u16) -> (ASTAddressingMode, ASTOperand) {
        if self.peek_token_is(0, TokenType::Comma) {
            self.next_token();
            if !self.peek_token_is(0, TokenType::Identifier) {
                panic!("Invalid hex operand");
            }
            self.next_token();
            match self.current_token.literal.to_uppercase().as_str() {
                "X" => (ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(word)),
                "Y" => (ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(word)),
                _ => panic!("Invalid AbsoluteX/Y operand"),
            }
        } else {
            (ASTAddressingMode::Absolute, ASTOperand::Absolute(word))
        }
    }

    #[tracing::instrument]
    fn parse_hex(&mut self, mnemonic: &ASTMnemonic) -> (ASTAddressingMode, ASTOperand) {
        // TODO: Make sure to not let 0x00FF be parsed as a byte!
        if let Some(byte) = self.try_parse_hex_u8() {
            self.parse_hex_byte(byte, mnemonic)
        } else if let Some(word) = self.try_parse_hex_u16() {
            self.parse_hex_word(word)
        } else {
            panic!("Invalid hex operand: {}", self.current_token.literal);
        }
    }

    #[tracing::instrument]
    fn parse_decimal(&mut self, mnemonic: &ASTMnemonic) -> (ASTAddressingMode, ASTOperand) {
        if let Ok(byte) = self.current_token.literal.parse::<u8>() {
            self.parse_hex_byte(byte, mnemonic)
        } else if let Ok(word) = self.current_token.literal.parse::<u16>() {
            self.parse_hex_word(word)
        } else {
            panic!("Invalid decimal operand: {}", self.current_token.literal);
        }
    }

    // (u8,X) - where u8 is a byte or a constant
    #[tracing::instrument]
    fn parse_indirect_indexed_x(
        &mut self,
        byte: Option<u8>,
        identifier: Option<String>,
    ) -> (ASTAddressingMode, ASTOperand) {
        self.next_token(); // Consume the comma
        if !self.peek_token_is(0, TokenType::Identifier) {
            panic!("Invalid indirect indexed X operand");
        }
        self.next_token(); // Consume the 'X'
        let operand = match self.current_token.literal.to_uppercase().as_str() {
            "X" => (
                ASTAddressingMode::IndirectIndexedX,
                if let Some(byte) = byte {
                    ASTOperand::ZeroPage(byte)
                } else if let Some(identifier) = identifier {
                    ASTOperand::Constant(identifier)
                } else {
                    panic!("Invalid indirect indexed X operand")
                },
            ),
            _ => panic!(
                "Invalid indirect indexed X operand, got {:#?}",
                self.current_token.literal
            ),
        };
        self.next_token(); // Consume the closing parenthesis
        operand
    }

    // (u8),Y - where u8 is a byte or a constant
    #[tracing::instrument]
    fn parse_indirect_indexed_y(
        &mut self,
        byte: Option<u8>,
        identifier: Option<String>,
    ) -> (ASTAddressingMode, ASTOperand) {
        self.next_token(); // Consume the closing parenthesis
        if !self.peek_token_is(0, TokenType::Comma) {
            panic!("Invalid indirect indexed Y operand. Expected ','")
        }
        self.next_token(); // Consume the comma
        self.next_token(); // Consume the 'Y' identifier
        match self.current_token.literal.to_uppercase().as_str() {
            "Y" => (
                ASTAddressingMode::IndirectIndexedY,
                if let Some(byte) = byte {
                    ASTOperand::ZeroPage(byte)
                } else if let Some(identifier) = identifier {
                    ASTOperand::Constant(identifier)
                } else {
                    panic!("Invalid indirect indexed Y operand")
                },
            ),
            _ => panic!(
                "Invalid indirect indexed Y operand, Expected 'Y', got {:#?}",
                self.current_token.literal
            ),
        }
    }

    // (u8,X) or (u8),Y - where u8 is a byte or a constant
    #[tracing::instrument]
    fn try_parse_indirect_indexed(&mut self) -> Option<(ASTAddressingMode, ASTOperand)> {
        let byte = {
            if let Some(byte) = self.try_parse_hex_u8() {
                Some(byte)
            } else if let Ok(byte) = self.current_token.literal.parse::<u8>() {
                Some(byte)
            } else {
                None
            }
        };
        let identifier = self.try_parse_identifier(); // 'X' or 'Y'

        if byte.is_some() || identifier.is_some() {
            if self.peek_token_is(0, TokenType::Comma)
                && self.peek_token_is(1, TokenType::Identifier)
            {
                Some(self.parse_indirect_indexed_x(byte, identifier))
            } else if self.peek_token_is(0, TokenType::ParenRight)
                && self.peek_token_is(1, TokenType::Comma)
            {
                Some(self.parse_indirect_indexed_y(byte, identifier))
            } else {
                None
            }
        } else {
            None
        }
    }

    // (u8,X) or (u8),Y - where u8 is a byte or a constant
    // or
    // (u16) or - where u16 is a word or a constant
    #[tracing::instrument]
    fn parse_indirect(&mut self) -> (ASTAddressingMode, ASTOperand) {
        self.next_token(); // Consume the opening parenthesis

        if let Some(indirect_indexed) = self.try_parse_indirect_indexed() {
            indirect_indexed
        } else {
            // Absolute indirect, i.e. ($BEEF)
            // TODO: Make sure to not let 0x00FF be parsed as a byte!
            if let Some(word) = self.try_parse_hex_u16() {
                // Hex
                self.next_token(); // Consume the closing parenthesis
                (ASTAddressingMode::Indirect, ASTOperand::Absolute(word))
            } else if let Ok(word) = self.current_token.literal.parse::<u16>() {
                // Decimal
                self.next_token(); // Consume the closing parenthesis
                (ASTAddressingMode::Indirect, ASTOperand::Absolute(word))
            } else if self.current_token_is(TokenType::Identifier) {
                // Constant
                let identifier = self.current_token.literal.clone();
                self.next_token(); // Consume the identifier
                (
                    ASTAddressingMode::Indirect,
                    ASTOperand::Constant(identifier),
                )
            } else {
                panic!("Invalid indirect operand")
            }
        }
    }

    #[tracing::instrument]
    fn addressing_mode_for_label(&self, mnemonic: &ASTMnemonic) -> ASTAddressingMode {
        // TODO: Is this correct?
        if mnemonic.is_branch() {
            ASTAddressingMode::Relative
        } else {
            ASTAddressingMode::Absolute
        }
    }

    #[tracing::instrument]
    fn parse_operand_with_identifier(
        &mut self,
        mnemonic: &ASTMnemonic,
    ) -> (ASTAddressingMode, ASTOperand) {
        if mnemonic.is_branch() || mnemonic.is_jump() {
            (
                self.addressing_mode_for_label(mnemonic),
                ASTOperand::Label(self.current_token.literal.clone()),
            )
        } else if self.peek_token_is(0, TokenType::Comma) {
            // ZeroPageX/Y with constant
            let constant = self.current_token.literal.clone();
            self.next_token(); // Consume the comma
            self.next_token(); // Consume the 'X' or 'Y'
            match self.current_token.literal.to_uppercase().as_str() {
                "X" => (ASTAddressingMode::ZeroPageX, ASTOperand::Constant(constant)),
                "Y" => (ASTAddressingMode::ZeroPageY, ASTOperand::Constant(constant)),
                _ => panic!(
                    "Invalid ZeroPageX/Y identifier, got {:#?}",
                    self.current_token.literal
                ),
            }
        } else {
            (
                // We don't know the size of the constant as this point (i.e. byte or word), so we
                // have to use the Constant addressing mode.
                // The compiler will later determine the size of the constant and switch to the
                // correct addressing mode, i.e. Absolute or ZeroPage.
                ASTAddressingMode::Constant,
                ASTOperand::Constant(self.current_token.literal.clone()),
            )
        }
    }

    #[tracing::instrument]
    fn peek_token_is_mnemonic(&self, peek_ahead: usize) -> bool {
        ASTMnemonic::from_str(self.peek_token(peek_ahead).literal.to_uppercase().as_str()).is_ok()
    }

    #[tracing::instrument]
    fn parse_addressing_mode_and_operand(
        &mut self,
        mnemonic: &ASTMnemonic,
    ) -> (ASTAddressingMode, ASTOperand) {
        if mnemonic.is_implied() {
            return (ASTAddressingMode::Implied, ASTOperand::Implied);
        }

        if mnemonic.has_accumulator_addressing_mode()
            && (self.peek_token_is_mnemonic(0) || self.peek_token_is(0, TokenType::Eof))
        {
            return (ASTAddressingMode::Accumulator, ASTOperand::Implied);
        }

        self.next_token();

        match self.current_token.token {
            TokenType::LiteralNumber => self.parse_literal_number(),
            TokenType::Hex => self.parse_hex(mnemonic),
            TokenType::Decimal => self.parse_decimal(mnemonic),
            TokenType::ParenLeft => self.parse_indirect(),
            TokenType::Identifier => self.parse_operand_with_identifier(mnemonic),
            _ => panic!("Invalid operand, got {:#?}", self.current_token.token),
        }
    }

    #[tracing::instrument]
    fn parse_instruction(&mut self) -> ASTInstructionNode {
        if self.current_token_is(TokenType::Identifier) {
            let mnemonic = self.parse_mnemonic();
            let (addr_mode, operand) = self.parse_addressing_mode_and_operand(&mnemonic);

            // eprintln!(
            //     "parsed mnemonic: {:#?}, addr_mode: {:#?}, operand: {:#?}",
            //     mnemonic, addr_mode, operand
            // );

            ASTInstructionNode::new(mnemonic, addr_mode, operand)
        } else {
            panic!("Expected identifier, got {:#?}", self.current_token.token);
        }
    }

    #[tracing::instrument]
    fn parse_constant(&mut self) -> ASTConstantNode {
        if !self.peek_token_is(0, TokenType::Identifier) {
            panic!("Expected identifier");
        }

        self.next_token(); // Consume the define keyword

        let identifier = self.current_token.literal.clone();
        self.next_token(); // Consume the identifier

        if self.current_token_is(TokenType::Hex) {
            if let Some(byte) = self.try_parse_hex_u8() {
                ASTConstantNode::new_byte(identifier, byte)
            // TODO: Make sure to not let 0x00FF be parsed as a byte!
            } else if let Some(word) = self.try_parse_hex_u16() {
                ASTConstantNode::new_word(identifier, word)
            } else {
                panic!("Invalid hex constant: {}", self.current_token.literal);
            }
        } else if self.current_token_is(TokenType::Decimal) {
            if let Ok(byte) = self.current_token.literal.parse::<u8>() {
                ASTConstantNode::new_byte(identifier, byte)
            } else if let Ok(word) = self.current_token.literal.parse::<u16>() {
                ASTConstantNode::new_word(identifier, word)
            } else {
                panic!("Invalid decimal constant: {}", self.current_token.literal);
            }
        } else {
            panic!(
                "Expected hex or decimal constant: {}",
                self.current_token.literal
            );
        }
    }

    #[tracing::instrument]
    fn parse_node(&mut self) -> ASTNode {
        match &self.current_token.token {
            TokenType::Identifier => {
                if self.peek_token_is(0, TokenType::Colon) {
                    ASTNode::Label(self.parse_label())
                } else {
                    ASTNode::Instruction(self.parse_instruction())
                }
            }
            TokenType::Define => ASTNode::Constant(self.parse_constant()),
            _ => panic!(
                "parse_node: Unexpected token type: {:#?}",
                self.current_token.token
            ),
        }
    }

    /// Parse the entire program into an AST
    #[tracing::instrument]
    pub fn parse_program(&mut self) -> AST {
        let mut ast_nodes = Vec::new();
        loop {
            if self.current_token_is(TokenType::Eof) {
                break;
            }

            ast_nodes.push(self.parse_node());
            self.next_token();
        }

        ast_nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assembler::lexer::Lexer;

    // ** Happy path tests **
    #[test]
    fn test_parser() {
        let input = "LDA #$C8\nSTA $0200";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        // Check that current token and lookahead tokens are loaded correctly
        assert_eq!(parser.current_token.token, TokenType::Identifier);
        assert_eq!(parser.peek_token(0).token, TokenType::LiteralNumber);
        assert!(parser.peek_token_is(0, TokenType::LiteralNumber));
        assert_eq!(parser.peek_token(1).token, TokenType::Hex);
        assert!(parser.peek_token_is(1, TokenType::Hex));

        parser.next_token(); // Consume the LDA token
        assert_eq!(parser.current_token.token, TokenType::LiteralNumber);
        assert_eq!(parser.peek_token(0).token, TokenType::Hex);
        assert_eq!(parser.peek_token(1).token, TokenType::Identifier);

        parser.next_token(); // Consume the literal number token
        parser.next_token(); // Consume the hex token
        assert_eq!(parser.current_token.token, TokenType::Identifier);
        assert_eq!(parser.peek_token(0).token, TokenType::Hex);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);

        parser.next_token(); // Consume the STA token
        assert_eq!(parser.current_token.token, TokenType::Hex);
        assert_eq!(parser.peek_token(0).token, TokenType::Eof);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);

        parser.next_token(); // Consume the Hex token
        parser.next_token(); // Consume the Eof token
        parser.next_token(); // Will only get eof tokens from now on
        assert_eq!(parser.current_token.token, TokenType::Eof);
        assert_eq!(parser.peek_token(0).token, TokenType::Eof);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);
    }

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
                "LDA #$C8",
                ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(0xC8),
                ),
            ),
            (
                "LDA #128",
                ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::Immediate,
                    ASTOperand::Immediate(128),
                ),
            ),
            (
                "ADC $BEEF",
                ASTInstructionNode::new(
                    ASTMnemonic::ADC,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(0xBEEF),
                ),
            ),
            (
                "ADC 65535",
                ASTInstructionNode::new(
                    ASTMnemonic::ADC,
                    ASTAddressingMode::Absolute,
                    ASTOperand::Absolute(65535),
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
                "ADC 128",
                ASTInstructionNode::new(
                    ASTMnemonic::ADC,
                    ASTAddressingMode::ZeroPage,
                    ASTOperand::ZeroPage(128),
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
                "INC 128,X",
                ASTInstructionNode::new(
                    ASTMnemonic::INC,
                    ASTAddressingMode::ZeroPageX,
                    ASTOperand::ZeroPage(128),
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
                "LDX 128,Y",
                ASTInstructionNode::new(
                    ASTMnemonic::LDX,
                    ASTAddressingMode::ZeroPageY,
                    ASTOperand::ZeroPage(128),
                ),
            ),
            // TODO: Enable test when fix for zero padded u16 is in place
            // (
            //     "CMP $00EF,X",
            //     ASTInstructionNode::new(
            //         ASTMnemonic::CMP,
            //         ASTAddressingMode::AbsoluteX,
            //         ASTOperand::Absolute(0x00EF),
            //     ),
            // ),
            (
                "CMP 65535,X",
                ASTInstructionNode::new(
                    ASTMnemonic::CMP,
                    ASTAddressingMode::AbsoluteX,
                    ASTOperand::Absolute(65535),
                ),
            ),
            (
                "EOR 65535,Y",
                ASTInstructionNode::new(
                    ASTMnemonic::EOR,
                    ASTAddressingMode::AbsoluteY,
                    ASTOperand::Absolute(65535),
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
                "BEQ 3",
                ASTInstructionNode::new(
                    ASTMnemonic::BEQ,
                    ASTAddressingMode::Relative,
                    ASTOperand::Relative(3),
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
                "JMP (65535)",
                ASTInstructionNode::new(
                    ASTMnemonic::JMP,
                    ASTAddressingMode::Indirect,
                    ASTOperand::Absolute(65535),
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
                "EOR (128,X)",
                ASTInstructionNode::new(
                    ASTMnemonic::EOR,
                    ASTAddressingMode::IndirectIndexedX,
                    ASTOperand::ZeroPage(128),
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
                "LDA (128),Y",
                ASTInstructionNode::new(
                    ASTMnemonic::LDA,
                    ASTAddressingMode::IndirectIndexedY,
                    ASTOperand::ZeroPage(128),
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
            (
                "LSR",
                ASTInstructionNode::new(
                    ASTMnemonic::LSR,
                    ASTAddressingMode::Accumulator,
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
                "define zero 0",
                vec![ASTNode::Constant(ASTConstantNode::new_byte(
                    "zero".to_string(),
                    0,
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
                "define sysRandom 53264",
                vec![ASTNode::Constant(ASTConstantNode::new_word(
                    "sysRandom".to_string(),
                    53264,
                ))],
            ),
            (
                "define sysRandom $d010\nLDY sysRandom",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_word("sysRandom".to_string(), 0xd010)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDY,
                        ASTAddressingMode::Constant,
                        ASTOperand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
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
                        ASTAddressingMode::Constant,
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
                "define zpage $02\nLDA zpage,Y",
                vec![
                    ASTNode::Constant(ASTConstantNode::new_byte("zpage".to_string(), 0x02)),
                    ASTNode::Instruction(ASTInstructionNode::new(
                        ASTMnemonic::LDA,
                        ASTAddressingMode::ZeroPageY,
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
        let input = "  define zero 0
  LDX #zero
  LDY #0
firstloop:
  TXA
  STA $0200,Y
  PHA
  INX
  INY
  CPY #16
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
                ASTOperand::Immediate(0),
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
                ASTOperand::Immediate(16),
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
    }
}
