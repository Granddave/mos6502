use std::{collections::VecDeque, str::FromStr};

use thiserror::Error;

use crate::{
    assembler::lexer::{token::Token, token::TokenType, Lexer, LexerError},
    ast::{AddressingMode, Constant, Directive, Instruction, Mnemonic, Node, Operand, AST},
};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid token - {0}:\n{1}")]
    InvalidToken(String, Token),
    #[error(transparent)]
    LexingError(#[from] LexerError),
}

// ParseError helpers.
// Takes a reference to a `Parser` and a format string with optional arguments.
macro_rules! invalid_token {
    // No format arguments
    ($parser:ident, $fmt:expr) => {
        ParseError::InvalidToken(format!($fmt), $parser.current_token.clone())
    };
    // With format arguments
    ($parser:ident, $fmt:expr, $($arg:tt)+) => {
        ParseError::InvalidToken(format!($fmt, $($arg)+), $parser.current_token.clone())
    };
}

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
    pub fn new(lexer: &'a mut Lexer<'a>) -> Result<Self, ParseError> {
        // Feed the lexer so its tokens are ready to be consumed

        let mut parser = Self {
            lexer,
            current_token: Token::default(),
            peek_tokens: VecDeque::with_capacity(PEEK_BUFFER_SIZE),
        };

        for _ in 0..PEEK_BUFFER_SIZE {
            parser.load_next_token()?;
        }

        parser.next_token()?; // Load the first token

        Ok(parser)
    }

    #[tracing::instrument]
    fn load_next_token(&mut self) -> Result<(), ParseError> {
        self.peek_tokens
            .push_back(self.lexer.next_token()?.unwrap());

        Ok(())
    }

    #[tracing::instrument]
    fn next_token(&mut self) -> Result<(), ParseError> {
        self.load_next_token()?;
        self.current_token = self.peek_tokens.pop_front().unwrap();
        Ok(())
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
    fn parse_label(&mut self) -> Result<String, ParseError> {
        if self.current_token_is(TokenType::Identifier) && self.peek_token_is(0, TokenType::Colon) {
            let label = self.current_token.lexeme.clone();
            self.next_token()?; // Consume the identifier
            Ok(label)
        } else {
            Err(invalid_token!(
                self,
                "expected identifier followed by colon"
            ))
        }
    }

    #[tracing::instrument]
    fn parse_mnemonic(&mut self) -> Result<Mnemonic, ParseError> {
        Mnemonic::from_str(self.current_token.lexeme.to_uppercase().as_str()).map_err(|_| {
            invalid_token!(self, "Invalid instruction mnemonic: {}", self.current_token)
        })
    }

    #[tracing::instrument]
    fn try_parse_hex_u8(&mut self) -> Option<u8> {
        let operand = self.current_token.lexeme.clone();
        let operand = operand.trim_start_matches('$');
        if operand.len() > 2 {
            return None;
        }
        match u8::from_str_radix(operand, 16) {
            Ok(byte) => Some(byte),
            Err(_) => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_hex_u16(&mut self) -> Option<u16> {
        let operand = self.current_token.lexeme.clone();
        let operand = operand.trim_start_matches('$');
        if operand.len() < 2 || operand.len() > 4 {
            return None;
        }
        match u16::from_str_radix(operand, 16) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_binary_u8(&mut self) -> Option<u8> {
        let operand = self.current_token.lexeme.clone();
        let operand = operand.trim_start_matches('%');
        if operand.len() > 8 {
            return None;
        }
        match u8::from_str_radix(operand, 2) {
            Ok(byte) => Some(byte),
            Err(_) => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_binary_u16(&mut self) -> Option<u16> {
        let operand = self.current_token.lexeme.clone();
        let operand = operand.trim_start_matches('%');
        if operand.len() < 8 || operand.len() > 16 {
            return None;
        }
        match u16::from_str_radix(operand, 2) {
            Ok(word) => Some(word),
            Err(_) => None,
        }
    }

    fn try_parse_decimal_u8(&mut self) -> Option<u8> {
        self.current_token.lexeme.parse::<u8>().ok()
    }

    fn try_parse_decimal_u16(&mut self) -> Option<u16> {
        self.current_token.lexeme.parse::<u16>().ok()
    }

    fn try_parse_u8(&mut self) -> Option<u8> {
        match self.current_token.token {
            TokenType::HexNumber => self.try_parse_hex_u8(),
            TokenType::BinaryNumber => self.try_parse_binary_u8(),
            TokenType::DecimalNumber => self.try_parse_decimal_u8(),
            _ => None,
        }
    }

    fn try_parse_u16(&mut self) -> Option<u16> {
        match self.current_token.token {
            TokenType::HexNumber => self.try_parse_hex_u16(),
            TokenType::BinaryNumber => self.try_parse_binary_u16(),
            TokenType::DecimalNumber => self.try_parse_decimal_u16(),
            _ => None,
        }
    }

    #[tracing::instrument]
    fn try_parse_identifier(&mut self) -> Option<String> {
        if self.current_token_is(TokenType::Identifier) {
            Some(self.current_token.lexeme.clone())
        } else {
            None
        }
    }

    /// Parse a literal number or identifier.
    ///
    /// The literal number is denoted by the following tokens:
    /// - Hex: #$xx
    /// - Binary: #%xx
    /// - Decimal: #xx
    /// - Identifier: #some_constant
    #[tracing::instrument]
    fn parse_literal_number(&mut self) -> Result<(AddressingMode, Operand), ParseError> {
        self.next_token()?; // Consume the literal token '#'
        if let Some(byte) = self.try_parse_u8() {
            Ok((AddressingMode::Immediate, Operand::Immediate(byte)))
        } else if let Some(identifier) = self.try_parse_identifier() {
            Ok((AddressingMode::Immediate, Operand::Constant(identifier)))
        } else {
            Err(invalid_token!(self, "invalid literal u8 or constant"))
        }
    }

    #[tracing::instrument]
    fn parse_byte_operand(
        &mut self,
        byte: u8,
        preceding_mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if self.peek_token_is(0, TokenType::Comma) {
            // zp,x or zp,y
            self.next_token()?; // Consume the byte
            if !self.peek_token_is(0, TokenType::Identifier) {
                return Err(invalid_token!(self, "invalid ZeroPageX/Y operand"));
            }
            self.next_token()?; // Consume the comma
            match self.current_token.lexeme.to_uppercase().as_str() {
                "X" => Ok((AddressingMode::ZeroPageX, Operand::ZeroPage(byte))),
                "Y" => Ok((AddressingMode::ZeroPageY, Operand::ZeroPage(byte))),
                _ => Err(invalid_token!(self, "invalid ZeroPageX/Y operand")),
            }
        } else if preceding_mnemonic.is_branching_instruction() {
            Ok((AddressingMode::Relative, Operand::Relative(byte as i8)))
        } else {
            Ok((AddressingMode::ZeroPage, Operand::ZeroPage(byte)))
        }
    }

    #[tracing::instrument]
    fn parse_word_operand(&mut self, word: u16) -> Result<(AddressingMode, Operand), ParseError> {
        if self.peek_token_is(0, TokenType::Comma) {
            // a,x or a,y
            self.next_token()?; // Consume the word
            if !self.peek_token_is(0, TokenType::Identifier) {
                return Err(invalid_token!(self, "invalid hex operand"));
            }
            self.next_token()?; // Consume the comma
            match self.current_token.lexeme.to_uppercase().as_str() {
                "X" => Ok((AddressingMode::AbsoluteX, Operand::Absolute(word))),
                "Y" => Ok((AddressingMode::AbsoluteY, Operand::Absolute(word))),
                _ => Err(invalid_token!(self, "invalid X/Y operand")),
            }
        } else {
            // Absolute
            Ok((AddressingMode::Absolute, Operand::Absolute(word)))
        }
    }

    #[tracing::instrument]
    fn parse_hex_operand(
        &mut self,
        mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if let Some(byte) = self.try_parse_hex_u8() {
            self.parse_byte_operand(byte, mnemonic)
        } else if let Some(word) = self.try_parse_hex_u16() {
            self.parse_word_operand(word)
        } else {
            Err(invalid_token!(self, "invalid hex operand"))
        }
    }

    #[tracing::instrument]
    fn parse_binary_operand(
        &mut self,
        mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if let Some(byte) = self.try_parse_binary_u8() {
            self.parse_byte_operand(byte, mnemonic)
        } else if let Some(word) = self.try_parse_binary_u16() {
            self.parse_word_operand(word)
        } else {
            Err(invalid_token!(self, "invalid binary operand"))
        }
    }

    #[tracing::instrument]
    fn parse_decimal_operand(
        &mut self,
        mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if let Some(byte) = self.try_parse_decimal_u8() {
            self.parse_byte_operand(byte, mnemonic)
        } else if let Some(word) = self.try_parse_decimal_u16() {
            self.parse_word_operand(word)
        } else {
            Err(invalid_token!(self, "invalid decimal operand"))
        }
    }

    // (u8,X) - where u8 is a byte or an identifier
    #[tracing::instrument]
    fn parse_indirect_indexed_x(
        &mut self,
        byte: Option<u8>,
        identifier: Option<String>,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        self.next_token()?; // Consume byte or identifier
        if !self.peek_token_is(0, TokenType::Identifier) {
            return Err(invalid_token!(
                self,
                "invalid indirect indexed X operand, expected 'X'"
            ));
        }
        self.next_token()?; // Consume comma
        let operand = match self.current_token.lexeme.to_uppercase().as_str() {
            "X" => Ok((
                AddressingMode::IndirectIndexedX,
                if let Some(byte) = byte {
                    Operand::ZeroPage(byte)
                } else if let Some(identifier) = identifier {
                    Operand::Constant(identifier)
                } else {
                    return Err(invalid_token!(self, "invalid indirect indexed X operand"));
                },
            )),
            _ => Err(invalid_token!(self, "Invalid indirect indexed X operand")),
        };
        self.next_token()?; // Consume 'X'
        operand
    }

    // (u8),Y - where u8 is a byte or an identifier
    #[tracing::instrument]
    fn parse_indirect_indexed_y(
        &mut self,
        byte: Option<u8>,
        identifier: Option<String>,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        self.next_token()?; // Consume byte or identifier
        if !self.peek_token_is(0, TokenType::Comma) {
            return Err(invalid_token!(
                self,
                "invalid indirect indexed Y operand, expected ','"
            ));
        }
        self.next_token()?; // Consume the closing parenthesis
        self.next_token()?; // Consume the comma
        match self.current_token.lexeme.to_uppercase().as_str() {
            "Y" => Ok((
                AddressingMode::IndirectIndexedY,
                if let Some(byte) = byte {
                    Operand::ZeroPage(byte)
                } else if let Some(identifier) = identifier {
                    Operand::Constant(identifier)
                } else {
                    return Err(invalid_token!(self, "invalid indirect indexed Y operand"));
                },
            )),
            _ => Err(invalid_token!(
                self,
                "Invalid indirect indexed Y operand, Expected 'Y'"
            )),
        }
    }

    // (u8,X) or (u8),Y - where u8 is a byte or a constant
    #[tracing::instrument]
    fn try_parse_indirect_indexed(
        &mut self,
    ) -> Result<Option<(AddressingMode, Operand)>, ParseError> {
        // Note: open parenthesis is already consumed

        // The operand can be a byte or a constant
        let byte = self.try_parse_u8();
        let constant_identifier = self.try_parse_identifier();
        if byte.is_none() && constant_identifier.is_none() {
            return Ok(None);
        }

        if self.peek_token_is(0, TokenType::Comma)
            && self.peek_token_is(1, TokenType::Identifier /* X */)
        {
            // (u8,X)
            Ok(Some(
                self.parse_indirect_indexed_x(byte, constant_identifier)?,
            ))
        } else if self.peek_token_is(0, TokenType::ParenRight)
            && self.peek_token_is(1, TokenType::Comma)
        {
            // (u8),Y
            Ok(Some(
                self.parse_indirect_indexed_y(byte, constant_identifier)?,
            ))
        } else {
            Ok(None)
        }
    }

    // (u16) - absolute indirect where u16 is a word or a constant
    #[tracing::instrument]
    fn parse_indirect_absolute(&mut self) -> Result<(AddressingMode, Operand), ParseError> {
        if let Some(word) = self.try_parse_u16() {
            self.next_token()?; // Consume the u16
            Ok((AddressingMode::Indirect, Operand::Absolute(word)))
        } else if let Some(identifier) = self.try_parse_identifier() {
            self.next_token()?; // Consume the constant
            Ok((AddressingMode::Indirect, Operand::Constant(identifier)))
        } else {
            Err(invalid_token!(self, "invalid indirect operand"))
        }
    }

    // (u8,X) or (u8),Y - indirect indexed where u8 is a byte or a constant
    // or
    // (u16) or - absolute indirect where u16 is a word or a constant
    #[tracing::instrument]
    fn parse_indirect(&mut self) -> Result<(AddressingMode, Operand), ParseError> {
        self.next_token()?; // Consume the opening parenthesis

        if let Some(indirect_indexed) = self.try_parse_indirect_indexed()? {
            Ok(indirect_indexed)
        } else {
            self.parse_indirect_absolute()
        }
    }

    #[tracing::instrument]
    fn addressing_mode_for_label(&self, mnemonic: &Mnemonic) -> AddressingMode {
        // TODO: Is this correct?
        if mnemonic.is_branching_instruction() {
            AddressingMode::Relative
        } else {
            AddressingMode::Absolute
        }
    }

    #[tracing::instrument]
    fn parse_operand_with_identifier(
        &mut self,
        mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if mnemonic.is_branching_instruction() || mnemonic.is_jumping_instruction() {
            Ok((
                self.addressing_mode_for_label(mnemonic),
                Operand::Label(self.current_token.lexeme.clone()),
            ))
        } else if self.peek_token_is(0, TokenType::Comma) {
            // zp,x or zp,y - where zp is a constant
            let constant = self.current_token.lexeme.clone();
            self.next_token()?; // Consume the constant
            self.next_token()?; // Consume the comma
            match self.current_token.lexeme.to_uppercase().as_str() {
                "X" => Ok((AddressingMode::ZeroPageX, Operand::Constant(constant))),
                "Y" => Ok((AddressingMode::ZeroPageY, Operand::Constant(constant))),
                _ => Err(invalid_token!(self, "invalid ZeroPageX/Y identifier")),
            }
        } else {
            Ok((
                // We don't know the size of the constant as this point (i.e. byte or word), so we
                // have to use the Constant addressing mode.
                // The compiler will later determine the size of the constant and switch to the
                // correct addressing mode, i.e. Absolute or ZeroPage.
                AddressingMode::Constant,
                Operand::Constant(self.current_token.lexeme.clone()),
            ))
        }
    }

    #[tracing::instrument]
    fn peek_token_is_mnemonic(&self, peek_ahead: usize) -> bool {
        Mnemonic::from_str(self.peek_token(peek_ahead).lexeme.to_uppercase().as_str()).is_ok()
    }

    #[tracing::instrument]
    fn parse_addressing_mode_and_operand(
        &mut self,
        mnemonic: &Mnemonic,
    ) -> Result<(AddressingMode, Operand), ParseError> {
        if mnemonic.has_implied_addressing_mode() {
            return Ok((AddressingMode::Implied, Operand::Implied));
        }

        if mnemonic.has_accumulator_addressing_mode()
            && (self.peek_token_is_mnemonic(0) || self.peek_token_is(0, TokenType::Eof))
        {
            return Ok((AddressingMode::Accumulator, Operand::Implied));
        }

        self.next_token()?; // Consume the mnemonic

        match self.current_token.token {
            TokenType::Pound => self.parse_literal_number(),
            TokenType::HexNumber => self.parse_hex_operand(mnemonic),
            TokenType::BinaryNumber => self.parse_binary_operand(mnemonic),
            TokenType::DecimalNumber => self.parse_decimal_operand(mnemonic),
            TokenType::ParenLeft => self.parse_indirect(),
            TokenType::Identifier => self.parse_operand_with_identifier(mnemonic),
            _ => Err(invalid_token!(self, "invalid operand")),
        }
    }

    #[tracing::instrument]
    fn parse_instruction(&mut self) -> Result<Instruction, ParseError> {
        if self.current_token_is(TokenType::Identifier) {
            let mnemonic = self.parse_mnemonic()?;
            let (addr_mode, operand) = self.parse_addressing_mode_and_operand(&mnemonic)?;

            Ok(Instruction::new(mnemonic, addr_mode, operand))
        } else {
            Err(invalid_token!(self, "expected a mnemonic identifier"))
        }
    }

    #[tracing::instrument]
    fn parse_constant(&mut self) -> Result<Constant, ParseError> {
        if !self.peek_token_is(0, TokenType::Identifier) {
            return Err(invalid_token!(self, "expected identifier, got"));
        }

        self.next_token()?; // Consume the define keyword

        let identifier = self.current_token.lexeme.clone();
        self.next_token()?; // Consume the identifier

        if let Some(byte) = self.try_parse_u8() {
            Ok(Constant::new_byte(identifier, byte))
        } else if let Some(word) = self.try_parse_u16() {
            Ok(Constant::new_word(identifier, word))
        } else {
            Err(invalid_token!(self, "expected integer literal"))
        }
    }

    #[tracing::instrument]
    fn parse_org_directive(&mut self) -> Result<Directive, ParseError> {
        self.next_token()?; // Consume the org keyword

        if let Some(word) = self.try_parse_u16() {
            Ok(Directive::Origin(word))
        } else {
            Err(invalid_token!(self, "expected hex number"))
        }
    }

    #[tracing::instrument]
    fn parse_directive(&mut self) -> Result<Directive, ParseError> {
        if !self.current_token_is(TokenType::Dot) {
            return Err(invalid_token!(self, "expected directive"));
        }

        self.next_token()?; // Consume the dot
        match self.current_token.token {
            TokenType::OrgDirective => Ok(self.parse_org_directive()?),
            _ => Err(invalid_token!(self, "invalid directive")),
        }
    }

    #[tracing::instrument]
    fn parse_node(&mut self) -> Result<Node, ParseError> {
        match &self.current_token.token {
            TokenType::Identifier => {
                if self.peek_token_is(0, TokenType::Colon) {
                    Ok(Node::Label(self.parse_label()?))
                } else {
                    Ok(Node::Instruction(self.parse_instruction()?))
                }
            }
            TokenType::Define => Ok(Node::Constant(self.parse_constant()?)),
            TokenType::Dot => Ok(Node::Directive(self.parse_directive()?)),
            _ => Err(invalid_token!(self, "start of node")),
        }
    }

    /// Parse the entire program into an AST
    #[tracing::instrument]
    pub fn parse_program(&mut self) -> Result<AST, ParseError> {
        let mut ast_nodes = Vec::new();
        loop {
            if self.current_token_is(TokenType::Eof) {
                break;
            }

            ast_nodes.push(self.parse_node()?);
            self.next_token()?; // Consume the last token of the node
        }

        Ok(ast_nodes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assembler::lexer::Lexer;

    use pretty_assertions::assert_eq;

    // ** Happy path tests **
    #[test]
    fn test_parser() -> Result<(), ParseError> {
        let input = "LDA #$C8\nSTA $0200";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer)?;

        // Check that current token and lookahead tokens are loaded correctly
        assert_eq!(parser.current_token.token, TokenType::Identifier);
        assert_eq!(parser.peek_token(0).token, TokenType::Pound);
        assert!(parser.peek_token_is(0, TokenType::Pound));
        assert_eq!(parser.peek_token(1).token, TokenType::HexNumber);
        assert!(parser.peek_token_is(1, TokenType::HexNumber));

        parser.next_token()?; // Consume the LDA token
        assert_eq!(parser.current_token.token, TokenType::Pound);
        assert_eq!(parser.peek_token(0).token, TokenType::HexNumber);
        assert_eq!(parser.peek_token(1).token, TokenType::Identifier);

        parser.next_token()?; // Consume the literal number token
        parser.next_token()?; // Consume the hex token
        assert_eq!(parser.current_token.token, TokenType::Identifier);
        assert_eq!(parser.peek_token(0).token, TokenType::HexNumber);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);

        parser.next_token()?; // Consume the STA token
        assert_eq!(parser.current_token.token, TokenType::HexNumber);
        assert_eq!(parser.peek_token(0).token, TokenType::Eof);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);

        parser.next_token()?; // Consume the Hex token
        parser.next_token()?; // Consume the Eof token
        parser.next_token()?; // Will only get eof tokens from now on
        assert_eq!(parser.current_token.token, TokenType::Eof);
        assert_eq!(parser.peek_token(0).token, TokenType::Eof);
        assert_eq!(parser.peek_token(1).token, TokenType::Eof);

        Ok(())
    }

    #[test]
    fn test_parse_label() -> Result<(), ParseError> {
        let tests = vec![("label:", "label".to_string())];

        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer)?;
            assert_eq!(parser.parse_label()?, expected);
        }
        Ok(())
    }

    #[test]
    fn test_parse_mnemonic() -> Result<(), ParseError> {
        // A subset of the 6502 mnemonics
        let tests = vec![
            ("LDA", Mnemonic::LDA),
            ("LDX", Mnemonic::LDX),
            ("LDY", Mnemonic::LDY),
            ("LSR", Mnemonic::LSR),
            ("NOP", Mnemonic::NOP),
            ("ORA", Mnemonic::ORA),
            ("PHA", Mnemonic::PHA),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer)?;
            assert_eq!(parser.parse_mnemonic()?, expected);
        }
        Ok(())
    }

    #[test]
    fn test_instruction() -> Result<(), ParseError> {
        let tests = vec![
            (
                // Immediate - small hex
                "LDA #$c8",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0xC8),
                ),
            ),
            (
                // Immediate - hex
                "LDA #$C8",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0xC8),
                ),
            ),
            (
                // Immediate - binary
                "LDA #%01010101",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(0b01010101),
                ),
            ),
            (
                // Immediate - decimal
                "LDA #128",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::Immediate,
                    Operand::Immediate(128),
                ),
            ),
            (
                // Absolute - hex
                "ADC $BEEF",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::Absolute,
                    Operand::Absolute(0xBEEF),
                ),
            ),
            (
                // Absolute - hex, word with leading zeros
                "ADC $ABC",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::Absolute,
                    Operand::Absolute(0x0ABC),
                ),
            ),
            (
                // Absolute - binary
                "ADC %1111111111111111",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::Absolute,
                    Operand::Absolute(0b1111111111111111),
                ),
            ),
            (
                // Absolute - decimal
                "ADC 65535",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::Absolute,
                    Operand::Absolute(65535),
                ),
            ),
            (
                // ZeroPage - hex
                "ADC $C8",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::ZeroPage,
                    Operand::ZeroPage(0xC8),
                ),
            ),
            (
                // ZeroPage - hex, byte with leading zeros
                "ADC $F",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::ZeroPage,
                    Operand::ZeroPage(0x0F),
                ),
            ),
            (
                // ZeroPage - binary
                "ADC %10000000",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::ZeroPage,
                    Operand::ZeroPage(0b10000000),
                ),
            ),
            (
                // ZeroPage - decimal
                "ADC 128",
                Instruction::new(
                    Mnemonic::ADC,
                    AddressingMode::ZeroPage,
                    Operand::ZeroPage(128),
                ),
            ),
            (
                // ZeroPageX - hex
                "INC $C8,X",
                Instruction::new(
                    Mnemonic::INC,
                    AddressingMode::ZeroPageX,
                    Operand::ZeroPage(0xC8),
                ),
            ),
            (
                // ZeroPageX - binary
                "INC %01010101,X",
                Instruction::new(
                    Mnemonic::INC,
                    AddressingMode::ZeroPageX,
                    Operand::ZeroPage(0b01010101),
                ),
            ),
            (
                // ZeroPageX - decimal
                "INC 128,X",
                Instruction::new(
                    Mnemonic::INC,
                    AddressingMode::ZeroPageX,
                    Operand::ZeroPage(128),
                ),
            ),
            (
                // ZeroPageY - hex
                "LDX $C8,Y",
                Instruction::new(
                    Mnemonic::LDX,
                    AddressingMode::ZeroPageY,
                    Operand::ZeroPage(0xC8),
                ),
            ),
            (
                // ZeroPageY - binary
                "LDX %01010101,Y",
                Instruction::new(
                    Mnemonic::LDX,
                    AddressingMode::ZeroPageY,
                    Operand::ZeroPage(0b01010101),
                ),
            ),
            (
                // ZeroPageY - decimal
                "LDX 128,Y",
                Instruction::new(
                    Mnemonic::LDX,
                    AddressingMode::ZeroPageY,
                    Operand::ZeroPage(128),
                ),
            ),
            (
                // AbsoluteX - hex
                "CMP $00EF,X",
                Instruction::new(
                    Mnemonic::CMP,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(0x00EF),
                ),
            ),
            (
                // AbsoluteX - binary
                "CMP %0101010101010101,X",
                Instruction::new(
                    Mnemonic::CMP,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(0b0101010101010101),
                ),
            ),
            (
                // AbsoluteX - decimal
                "CMP 65535,X",
                Instruction::new(
                    Mnemonic::CMP,
                    AddressingMode::AbsoluteX,
                    Operand::Absolute(65535),
                ),
            ),
            (
                // AbsoluteY - decimal
                "EOR 65535,Y",
                Instruction::new(
                    Mnemonic::EOR,
                    AddressingMode::AbsoluteY,
                    Operand::Absolute(65535),
                ),
            ),
            (
                // Relative - hex
                "BEQ $03",
                Instruction::new(
                    Mnemonic::BEQ,
                    AddressingMode::Relative,
                    Operand::Relative(0x03),
                ),
            ),
            (
                // Relative - binary
                "BEQ %00000011",
                Instruction::new(
                    Mnemonic::BEQ,
                    AddressingMode::Relative,
                    Operand::Relative(3),
                ),
            ),
            (
                // Relative - decimal
                "BEQ 3",
                Instruction::new(
                    Mnemonic::BEQ,
                    AddressingMode::Relative,
                    Operand::Relative(3),
                ),
            ),
            (
                // Indirect - hex
                "JMP ($BEEF)",
                Instruction::new(
                    Mnemonic::JMP,
                    AddressingMode::Indirect,
                    Operand::Absolute(0xBEEF),
                ),
            ),
            (
                // Indirect - binary
                "JMP (%1111111111111111)",
                Instruction::new(
                    Mnemonic::JMP,
                    AddressingMode::Indirect,
                    Operand::Absolute(65535),
                ),
            ),
            (
                // Indirect - decimal
                "JMP (65535)",
                Instruction::new(
                    Mnemonic::JMP,
                    AddressingMode::Indirect,
                    Operand::Absolute(65535),
                ),
            ),
            (
                // Indirect indexed X - hex
                "EOR ($C8,X)",
                Instruction::new(
                    Mnemonic::EOR,
                    AddressingMode::IndirectIndexedX,
                    Operand::ZeroPage(0xC8),
                ),
            ),
            (
                // Indirect indexed X - binary
                "EOR (%01010101,X)",
                Instruction::new(
                    Mnemonic::EOR,
                    AddressingMode::IndirectIndexedX,
                    Operand::ZeroPage(0b01010101),
                ),
            ),
            (
                // Indirect indexed X - decimal
                "EOR (128,X)",
                Instruction::new(
                    Mnemonic::EOR,
                    AddressingMode::IndirectIndexedX,
                    Operand::ZeroPage(128),
                ),
            ),
            (
                // Indirect indexed Y - hex
                "LDA ($C8),Y",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::IndirectIndexedY,
                    Operand::ZeroPage(0xC8),
                ),
            ),
            (
                // Indirect indexed Y - binary
                "LDA (%01010101),Y",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::IndirectIndexedY,
                    Operand::ZeroPage(0b01010101),
                ),
            ),
            (
                // Indirect indexed Y - decimal
                "LDA (128),Y",
                Instruction::new(
                    Mnemonic::LDA,
                    AddressingMode::IndirectIndexedY,
                    Operand::ZeroPage(128),
                ),
            ),
            (
                // Implied
                "BRK",
                Instruction::new(Mnemonic::BRK, AddressingMode::Implied, Operand::Implied),
            ),
            (
                // Accumulator
                "LSR",
                Instruction::new(Mnemonic::LSR, AddressingMode::Accumulator, Operand::Implied),
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer)?;
            assert_eq!(parser.parse_instruction()?, expected);
        }
        Ok(())
    }

    #[test]
    fn test_parse_constant() -> Result<(), ParseError> {
        let tests = vec![
            (
                // byte hex
                "define val $FE",
                vec![Node::Constant(Constant::new_byte("val".to_string(), 0xFE))],
            ),
            (
                // byte binary
                "define val %01010101",
                vec![Node::Constant(Constant::new_byte(
                    "val".to_string(),
                    0b01010101,
                ))],
            ),
            (
                // byte decimal
                "define val 32",
                vec![Node::Constant(Constant::new_byte("val".to_string(), 32))],
            ),
            (
                // word hex
                "define sysRandom $d010",
                vec![Node::Constant(Constant::new_word(
                    "sysRandom".to_string(),
                    0xd010,
                ))],
            ),
            (
                // word decimal
                "define sysRandom %0101010101010101",
                vec![Node::Constant(Constant::new_word(
                    "sysRandom".to_string(),
                    0b0101010101010101,
                ))],
            ),
            (
                // word decimal
                "define sysRandom 53264",
                vec![Node::Constant(Constant::new_word(
                    "sysRandom".to_string(),
                    53264,
                ))],
            ),
            // Different usages
            (
                "define sysRandom $d010\nLDY sysRandom",
                vec![
                    Node::Constant(Constant::new_word("sysRandom".to_string(), 0xd010)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDY,
                        AddressingMode::Constant,
                        Operand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define sysRandom $d010\nLDY (sysRandom)",
                vec![
                    Node::Constant(Constant::new_word("sysRandom".to_string(), 0xd010)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDY,
                        AddressingMode::Indirect,
                        Operand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define sysRandom $d010\nLDY (sysRandom,X)",
                vec![
                    Node::Constant(Constant::new_word("sysRandom".to_string(), 0xd010)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDY,
                        AddressingMode::IndirectIndexedX,
                        Operand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define sysRandom $d010\nLDY (sysRandom),Y",
                vec![
                    Node::Constant(Constant::new_word("sysRandom".to_string(), 0xd010)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDY,
                        AddressingMode::IndirectIndexedY,
                        Operand::Constant("sysRandom".to_string()),
                    )),
                ],
            ),
            (
                "define a_dozen $0c\nLDX #a_dozen",
                vec![
                    Node::Constant(Constant::new_byte("a_dozen".to_string(), 0x0c)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDX,
                        AddressingMode::Immediate,
                        Operand::Constant("a_dozen".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nLDA zpage",
                vec![
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::Constant,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nLDA zpage,X",
                vec![
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::ZeroPageX,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nLDA zpage,Y",
                vec![
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::LDA,
                        AddressingMode::ZeroPageY,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nSTA (zpage,X)",
                vec![
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::STA,
                        AddressingMode::IndirectIndexedX,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
            ),
            (
                "define zpage $02\nSTA (zpage),Y",
                vec![
                    Node::Constant(Constant::new_byte("zpage".to_string(), 0x02)),
                    Node::Instruction(Instruction::new(
                        Mnemonic::STA,
                        AddressingMode::IndirectIndexedY,
                        Operand::Constant("zpage".to_string()),
                    )),
                ],
            ),
        ];

        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer)?;
            assert_eq!(parser.parse_program()?, expected);
        }
        Ok(())
    }

    #[test]
    fn test_parse_program() -> Result<(), ParseError> {
        let input = "
  define zero 0
  define some_constant %01010101

.org $8000

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
            Node::Constant(Constant::new_byte("zero".to_string(), 0x00)),
            Node::Constant(Constant::new_byte("some_constant".to_string(), 0b01010101)),
            Node::Directive(Directive::Origin(0x8000)),
            Node::Instruction(Instruction::new(
                Mnemonic::LDX,
                AddressingMode::Immediate,
                Operand::Constant("zero".to_string()),
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::LDY,
                AddressingMode::Immediate,
                Operand::Immediate(0),
            )),
            Node::Label("firstloop".to_string()),
            Node::Instruction(Instruction::new(
                Mnemonic::TXA,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::STA,
                AddressingMode::AbsoluteY,
                Operand::Absolute(0x0200),
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::PHA,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::INX,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::INY,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::CPY,
                AddressingMode::Immediate,
                Operand::Immediate(16),
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::BNE,
                AddressingMode::Relative,
                Operand::Label("firstloop".to_string()),
            )),
            Node::Label("secondloop".to_string()),
            Node::Instruction(Instruction::new(
                Mnemonic::PLA,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::STA,
                AddressingMode::AbsoluteY,
                Operand::Absolute(0x0200),
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::INY,
                AddressingMode::Implied,
                Operand::Implied,
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::CPY,
                AddressingMode::Immediate,
                Operand::Immediate(0x20),
            )),
            Node::Instruction(Instruction::new(
                Mnemonic::BNE,
                AddressingMode::Relative,
                Operand::Label("secondloop".to_string()),
            )),
        ];
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer)?;
        assert_eq!(parser.parse_program()?, expected);
        Ok(())
    }
}
