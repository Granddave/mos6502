use thiserror::Error;

use self::{
    source_position::SourcePositionSpan,
    token::{Token, TokenType},
};
use source_position::SourcePosition;

pub mod source_position;
pub mod token;

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    #[error("Unexpected character at {0}: '{1}'")]
    UnexpectedCharacter(SourcePosition, char),
}

/// Lexer is used to tokenize source code.
#[derive(Debug)]
pub struct Lexer<'a> {
    // TODO: Add filename here?
    /// Source code to lex
    src: &'a str,
    /// Current char under examination
    ch: Option<char>,
    /// Index of current position in source code (points to current char)
    current_src_index: usize,
    /// Index of current reading position in source code (after current char)
    next_src_index: usize,
    /// Current line and column number in source code
    current_source_position: SourcePosition,
    /// Next line and column number in source code
    next_source_position: SourcePosition,
}

impl<'a> Lexer<'a> {
    #[tracing::instrument]
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Self {
            src,
            ch: None,
            current_src_index: 0,
            next_src_index: 0,
            current_source_position: SourcePosition::default(),
            next_source_position: SourcePosition::default(),
        };
        lexer.read_char();
        lexer
    }

    #[tracing::instrument]
    #[inline(always)]
    fn read_char(&mut self) {
        self.ch = self.src.chars().nth(self.next_src_index);
        self.current_src_index = self.next_src_index;
        self.current_source_position = self.next_source_position;
        if self.ch == Some('\n') {
            self.next_source_position.increment_line();
        } else {
            self.next_source_position.increment_column();
        }
        self.next_src_index += 1;
    }

    #[tracing::instrument]
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if !ch.is_whitespace() {
                break;
            }

            self.read_char();
        }
    }

    #[tracing::instrument]
    #[inline(always)]
    fn skip_comment(&mut self) {
        while self.ch.is_some() && self.ch.expect("ch should be some") != '\n' {
            self.read_char();
        }
    }

    #[tracing::instrument]
    fn read_while(&mut self, condition: fn(char) -> bool) -> (&str, SourcePositionSpan) {
        let start_src_index = self.current_src_index;
        let start_position = self.current_source_position;
        while self.ch.is_some() && condition(self.ch.expect("ch should be some")) {
            self.read_char();
        }
        (
            &self.src[start_src_index..self.current_src_index],
            SourcePositionSpan::new(start_position, self.current_source_position),
        )
    }

    #[tracing::instrument]
    fn read_one_char(&mut self) -> (&str, SourcePositionSpan) {
        let start_src_index = self.current_src_index;
        let start_position = self.current_source_position;
        if self.ch.is_some() {
            self.read_char();
        }
        (
            &self.src[start_src_index..self.current_src_index],
            SourcePositionSpan::new(start_position, self.current_source_position),
        )
    }

    #[tracing::instrument]
    fn consume_one_char(&mut self) -> SourcePositionSpan {
        let start_position = self.current_source_position;
        if self.ch.is_some() {
            self.read_char();
        }
        SourcePositionSpan::new(start_position, self.current_source_position)
    }

    #[tracing::instrument]
    fn read_string(&mut self) -> (&str, SourcePositionSpan) {
        self.read_while(|ch| ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_')
    }

    #[tracing::instrument]
    fn read_hex(&mut self) -> (&str, SourcePositionSpan) {
        self.read_while(|ch| ch.is_ascii_hexdigit())
    }

    #[tracing::instrument]
    fn read_binary(&mut self) -> (&str, SourcePositionSpan) {
        self.read_while(|ch| ch == '0' || ch == '1')
    }

    #[tracing::instrument]
    fn read_decimal(&mut self) -> (&str, SourcePositionSpan) {
        self.read_while(|ch| ch.is_ascii_digit())
    }

    #[tracing::instrument]
    fn read_binary_literal(&mut self, ch: char) -> Result<Token, LexerError> {
        let span_1 = self.consume_one_char(); // Consume '%'
        let (text, span_2) = self.read_binary();
        if text.is_empty() {
            return Err(LexerError::UnexpectedCharacter(
                self.current_source_position,
                ch,
            ));
        }
        Ok(Token::new(
            TokenType::BinaryNumber,
            text,
            SourcePositionSpan::new(span_1.start, span_2.end),
        ))
    }

    #[tracing::instrument]
    fn read_hex_literal(&mut self, ch: char) -> Result<Token, LexerError> {
        let span_1 = self.consume_one_char(); // Consume '$'
        let (text, span_2) = self.read_hex();
        if text.is_empty() {
            return Err(LexerError::UnexpectedCharacter(
                self.current_source_position,
                ch,
            ));
        }
        Ok(Token::new(
            TokenType::HexNumber,
            text,
            SourcePositionSpan::new(span_1.start, span_2.end),
        ))
    }

    fn read_string_literal(&mut self) -> Result<Token, LexerError> {
        let start_span = self.consume_one_char(); // Consume opening quote
        let (text, _) = self.read_while(|c| c != '"');
        let text = text.to_string(); // Convert to String to own the data
        let end_span = self.consume_one_char(); // Consume closing quote
        Ok(Token::new(
            TokenType::StringLiteral,
            text.as_str(),
            SourcePositionSpan::new(start_span.start, end_span.end),
        ))
    }

    #[tracing::instrument]
    pub fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        self.skip_whitespace();
        let token = match self.ch {
            Some(ch) => match ch {
                ';' => {
                    // TODO: Add comments as tokens instead of skipping them
                    self.skip_comment();
                    self.next_token()?
                }
                // Single character tokens
                '.' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::Dot, text, span))
                }
                ':' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::Colon, text, span))
                }
                ',' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::Comma, text, span))
                }
                '(' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::ParenLeft, text, span))
                }
                ')' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::ParenRight, text, span))
                }
                '#' => {
                    let (text, span) = self.read_one_char();
                    Some(Token::new(TokenType::Pound, text, span))
                }
                // Number literals
                '$' => Some(self.read_hex_literal(ch)?),
                '%' => Some(self.read_binary_literal(ch)?),
                '0'..='9' => {
                    let (text, span) = self.read_decimal();
                    Some(Token::new(TokenType::DecimalNumber, text, span))
                }
                // String literals
                '"' => Some(self.read_string_literal()?),
                // Identifiers and keywords
                'A'..='Z' | 'a'..='z' | '_' => {
                    let (text, span) = self.read_string();
                    match text {
                        "define" => Some(Token::new(TokenType::Define, text, span)),
                        "org" => Some(Token::new(TokenType::OrgDirective, text, span)),
                        "byte" => Some(Token::new(TokenType::ByteDirective, text, span)),
                        "word" => Some(Token::new(TokenType::WordDirective, text, span)),
                        _ => Some(Token::new(TokenType::Identifier, text, span)),
                    }
                }
                _ => {
                    return Err(LexerError::UnexpectedCharacter(
                        self.current_source_position,
                        ch,
                    ));
                }
            },
            None => Some(Token::new(
                TokenType::Eof,
                "",
                SourcePositionSpan::new(self.current_source_position, self.current_source_position),
            )),
        };
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Ok;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_hex() -> anyhow::Result<()> {
        let tests = vec![
            ("$0", "0"),
            ("$00", "00"),
            ("$01", "01"),
            ("$10", "10"),
            ("$ff", "ff"),
            ("$FF", "FF"),
            ("$FFFF", "FFFF"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token()?.unwrap();
            assert_eq!(token.token, TokenType::HexNumber);
            assert_eq!(token.lexeme, expected);
        }
        Ok(())
    }

    #[test]
    fn test_binary() -> anyhow::Result<()> {
        let tests = vec![
            ("%0", "0"),
            ("%00", "00"),
            ("%0000", "0000"),
            ("%0101", "0101"),
            ("%01010101", "01010101"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token()?.unwrap();
            assert_eq!(token.token, TokenType::BinaryNumber);
            assert_eq!(token.lexeme, expected);
        }
        Ok(())
    }

    #[test]
    fn test_invalid_hex_and_binary() -> anyhow::Result<()> {
        let tests = vec![
            (
                "$",
                LexerError::UnexpectedCharacter(SourcePosition::new(1, 2), '$'),
            ),
            (
                "%",
                LexerError::UnexpectedCharacter(SourcePosition::new(1, 2), '%'),
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let result = lexer.next_token();
            assert_eq!(result, Err(expected));
        }
        Ok(())
    }

    #[test]
    fn test_decimal() -> anyhow::Result<()> {
        let tests = vec![
            ("0", "0"),
            ("1", "1"),
            ("10", "10"),
            ("123", "123"),
            ("65535", "65535"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token()?.unwrap();
            assert_eq!(token.token, TokenType::DecimalNumber);
            assert_eq!(token.lexeme, expected);
        }
        Ok(())
    }

    #[test]
    fn test_identifier() -> anyhow::Result<()> {
        let tests = vec![("LDA", "LDA"), ("X", "X"), ("my_label", "my_label")];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token()?.unwrap();
            assert_eq!(token.token, TokenType::Identifier);
            assert_eq!(token.lexeme, expected);
        }
        Ok(())
    }

    #[test]
    fn test_instruction() -> anyhow::Result<()> {
        let tests = vec![
            (
                "LDA #$00",
                vec![
                    Token {
                        token: TokenType::Identifier,
                        lexeme: "LDA".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 1),
                            SourcePosition::new(1, 4),
                        ),
                    },
                    Token {
                        token: TokenType::Pound,
                        lexeme: "#".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 5),
                            SourcePosition::new(1, 6),
                        ),
                    },
                    Token {
                        token: TokenType::HexNumber,
                        lexeme: "00".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 6),
                            SourcePosition::new(1, 9),
                        ),
                    },
                    Token {
                        token: TokenType::Eof,
                        lexeme: "".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 9),
                            SourcePosition::new(1, 9),
                        ),
                    },
                ],
            ),
            (
                "LDA #%01010101",
                vec![
                    Token {
                        token: TokenType::Identifier,
                        lexeme: "LDA".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 1),
                            SourcePosition::new(1, 4),
                        ),
                    },
                    Token {
                        token: TokenType::Pound,
                        lexeme: "#".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 5),
                            SourcePosition::new(1, 6),
                        ),
                    },
                    Token {
                        token: TokenType::BinaryNumber,
                        lexeme: "01010101".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 6),
                            SourcePosition::new(1, 15),
                        ),
                    },
                    Token {
                        token: TokenType::Eof,
                        lexeme: "".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 15),
                            SourcePosition::new(1, 15),
                        ),
                    },
                ],
            ),
            (
                "LDA #255",
                vec![
                    Token {
                        token: TokenType::Identifier,
                        lexeme: "LDA".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 1),
                            SourcePosition::new(1, 4),
                        ),
                    },
                    Token {
                        token: TokenType::Pound,
                        lexeme: "#".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 5),
                            SourcePosition::new(1, 6),
                        ),
                    },
                    Token {
                        token: TokenType::DecimalNumber,
                        lexeme: "255".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 6),
                            SourcePosition::new(1, 9),
                        ),
                    },
                    Token {
                        token: TokenType::Eof,
                        lexeme: "".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 9),
                            SourcePosition::new(1, 9),
                        ),
                    },
                ],
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            loop {
                let token = lexer.next_token()?.unwrap();
                tokens.push(token.clone());
                if token.token == TokenType::Eof {
                    break;
                }
            }
            assert_eq!(tokens, expected);
        }
        Ok(())
    }

    #[test]
    fn test_label() -> anyhow::Result<()> {
        let input = "my_label:";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                lexeme: "my_label".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(1, 1), SourcePosition::new(1, 9)),
            },
            Token {
                token: TokenType::Colon,
                lexeme: ":".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 9),
                    SourcePosition::new(1, 10),
                ),
            },
            Token {
                token: TokenType::Eof,
                lexeme: "".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 10),
                    SourcePosition::new(1, 10),
                ),
            },
        ];
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?.unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
        Ok(())
    }

    #[test]
    fn test_define() -> anyhow::Result<()> {
        let input = "define   my_constant $FE";
        let result = vec![
            Token {
                token: TokenType::Define,
                lexeme: "define".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(1, 1), SourcePosition::new(1, 7)),
            },
            Token {
                token: TokenType::Identifier,
                lexeme: "my_constant".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 10),
                    SourcePosition::new(1, 21),
                ),
            },
            Token {
                token: TokenType::HexNumber,
                lexeme: "FE".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 22),
                    SourcePosition::new(1, 25),
                ),
            },
            Token {
                token: TokenType::Eof,
                lexeme: "".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 25),
                    SourcePosition::new(1, 25),
                ),
            },
        ];
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?.unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
        Ok(())
    }

    #[test]
    fn test_comment() -> anyhow::Result<()> {
        let input = "; this is a comment
LDA #$00 ; Another one
;Last one";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                lexeme: "LDA".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(2, 1), SourcePosition::new(2, 4)),
            },
            Token {
                token: TokenType::Pound,
                lexeme: "#".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(2, 5), SourcePosition::new(2, 6)),
            },
            Token {
                token: TokenType::HexNumber,
                lexeme: "00".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(2, 6), SourcePosition::new(2, 9)),
            },
            Token {
                token: TokenType::Eof,
                lexeme: "".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(3, 10),
                    SourcePosition::new(3, 10),
                ),
            },
        ];
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?.unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
        Ok(())
    }

    #[test]
    fn test_paren() -> anyhow::Result<()> {
        let input = "LDA ($D2,X)";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                lexeme: "LDA".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(1, 1), SourcePosition::new(1, 4)),
            },
            Token {
                token: TokenType::ParenLeft,
                lexeme: "(".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(1, 5), SourcePosition::new(1, 6)),
            },
            Token {
                token: TokenType::HexNumber,
                lexeme: "D2".to_string(),
                span: SourcePositionSpan::new(SourcePosition::new(1, 6), SourcePosition::new(1, 9)),
            },
            Token {
                token: TokenType::Comma,
                lexeme: ",".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 9),
                    SourcePosition::new(1, 10),
                ),
            },
            Token {
                token: TokenType::Identifier,
                lexeme: "X".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 10),
                    SourcePosition::new(1, 11),
                ),
            },
            Token {
                token: TokenType::ParenRight,
                lexeme: ")".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 11),
                    SourcePosition::new(1, 12),
                ),
            },
            Token {
                token: TokenType::Eof,
                lexeme: "".to_string(),
                span: SourcePositionSpan::new(
                    SourcePosition::new(1, 12),
                    SourcePosition::new(1, 12),
                ),
            },
        ];
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?.unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
        Ok(())
    }

    #[test]
    fn test_string_literal() -> anyhow::Result<()> {
        let tests = vec![
            (
                "\"a string literal\"",
                vec![
                    Token {
                        token: TokenType::StringLiteral,
                        lexeme: "a string literal".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 1),
                            SourcePosition::new(1, 19),
                        ),
                    },
                    Token {
                        token: TokenType::Eof,
                        lexeme: "".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 19),
                            SourcePosition::new(1, 19),
                        ),
                    },
                ],
            ),
            (
                "\"multi\nline\nstring\"",
                vec![
                    Token {
                        token: TokenType::StringLiteral,
                        lexeme: "multi\nline\nstring".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(1, 1),
                            SourcePosition::new(3, 8),
                        ),
                    },
                    Token {
                        token: TokenType::Eof,
                        lexeme: "".to_string(),
                        span: SourcePositionSpan::new(
                            SourcePosition::new(3, 8),
                            SourcePosition::new(3, 8),
                        ),
                    },
                ],
            ),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            loop {
                let token = lexer.next_token()?.unwrap();
                tokens.push(token.clone());
                if token.token == TokenType::Eof {
                    break;
                }
            }
            assert_eq!(tokens, expected);
        }
        Ok(())
    }
}
