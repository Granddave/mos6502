use thiserror::Error;

use self::token::{Token, TokenType};

pub mod token;

#[derive(Error, Debug)]
pub enum LexingError {
    #[error("Unexpected character on line {0}: '{1}'")]
    UnexpectedCharacter(usize, char),
}

/// Lexer is used to tokenize source code.
#[derive(Debug)]
pub struct Lexer<'a> {
    /// Source code to lex
    src: &'a str,
    /// Index of current position in source code (points to current char)
    position: usize,
    /// Index of current reading position in source code (after current char)
    read_position: usize,
    /// Current char under examination
    ch: Option<char>,
    /// Current line number in source code
    line_number: usize,
}

impl<'a> Lexer<'a> {
    #[tracing::instrument]
    pub fn new(src: &'a str) -> Self {
        let mut lexer = Self {
            src,
            position: 0,
            read_position: 0,
            ch: None,
            line_number: 1,
        };
        lexer.read_char();
        lexer
    }

    #[tracing::instrument]
    #[inline(always)]
    fn read_char(&mut self) {
        if self.read_position >= self.src.len() {
            self.ch = None;
        } else {
            self.ch = Some(
                self.src
                    .chars()
                    .nth(self.read_position)
                    .expect("Index should be valid"),
            );
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    #[tracing::instrument]
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        while self.ch.is_some() {
            let ch = self.ch.expect("ch should be some");
            if !ch.is_whitespace() {
                break;
            }

            if ch == '\n' {
                self.line_number += 1;
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
    fn read_while_condition(&mut self, condition: fn(char) -> bool) -> String {
        let position = self.position;
        while self.ch.is_some() && condition(self.ch.expect("ch should be some")) {
            self.read_char();
        }
        self.src[position..self.position].to_string()
    }

    #[tracing::instrument]
    fn read_string(&mut self) -> String {
        self.read_while_condition(|ch| ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_')
    }

    #[tracing::instrument]
    fn read_hex(&mut self) -> String {
        self.read_while_condition(|ch| ch.is_ascii_hexdigit())
    }

    #[tracing::instrument]
    fn read_binary(&mut self) -> String {
        self.read_while_condition(|ch| ch == '0' || ch == '1')
    }

    #[tracing::instrument]
    fn read_decimal(&mut self) -> String {
        self.read_while_condition(|ch| ch.is_ascii_digit())
    }

    #[tracing::instrument]
    fn create_token(&mut self, token: TokenType, literal: &str) -> Token {
        Token::new(token, literal, self.line_number)
    }

    #[tracing::instrument]
    pub fn next_token(&mut self) -> Result<Option<Token>, LexingError> {
        self.skip_whitespace();
        let token = match self.ch {
            Some(ch) => match ch {
                ';' => {
                    self.skip_comment();
                    self.next_token()?
                }
                '$' => {
                    self.read_char();
                    let hex = self.read_hex();
                    Some(self.create_token(TokenType::Hex, &hex))
                }
                '%' => {
                    self.read_char();
                    let binary = self.read_binary();
                    Some(self.create_token(TokenType::Binary, &binary))
                }
                '0'..='9' => {
                    let decimal = self.read_decimal();
                    Some(self.create_token(TokenType::Decimal, &decimal))
                }
                '#' => {
                    self.read_char();
                    Some(self.create_token(TokenType::LiteralNumber, "#"))
                }
                ':' => {
                    self.read_char();
                    Some(self.create_token(TokenType::Colon, ":"))
                }
                ',' => {
                    self.read_char();
                    Some(self.create_token(TokenType::Comma, ","))
                }
                '(' => {
                    self.read_char();
                    Some(self.create_token(TokenType::ParenLeft, "("))
                }
                ')' => {
                    self.read_char();
                    Some(self.create_token(TokenType::ParenRight, ")"))
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let identifier = self.read_string();
                    if identifier == "define" {
                        Some(self.create_token(TokenType::Define, &identifier))
                    } else {
                        Some(self.create_token(TokenType::Identifier, &identifier))
                    }
                }
                _ => {
                    return Err(LexingError::UnexpectedCharacter(self.line_number, ch));
                }
            },
            None => Some(self.create_token(TokenType::Eof, "")),
        };
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Ok;

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
            assert_eq!(token.token, TokenType::Hex);
            assert_eq!(token.literal, expected);
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
            assert_eq!(token.token, TokenType::Binary);
            assert_eq!(token.literal, expected);
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
            assert_eq!(token.token, TokenType::Decimal);
            assert_eq!(token.literal, expected);
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
            assert_eq!(token.literal, expected);
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
                        literal: "LDA".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::LiteralNumber,
                        literal: "#".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Hex,
                        literal: "00".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Eof,
                        literal: "".to_string(),
                        line_number: 1,
                    },
                ],
            ),
            (
                "LDA #%01010101",
                vec![
                    Token {
                        token: TokenType::Identifier,
                        literal: "LDA".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::LiteralNumber,
                        literal: "#".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Binary,
                        literal: "01010101".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Eof,
                        literal: "".to_string(),
                        line_number: 1,
                    },
                ],
            ),
            (
                "LDA #255",
                vec![
                    Token {
                        token: TokenType::Identifier,
                        literal: "LDA".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::LiteralNumber,
                        literal: "#".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Decimal,
                        literal: "255".to_string(),
                        line_number: 1,
                    },
                    Token {
                        token: TokenType::Eof,
                        literal: "".to_string(),
                        line_number: 1,
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
                literal: "my_label".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Colon,
                literal: ":".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
                line_number: 1,
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
        let input = "define my_constant $FE";
        let result = vec![
            Token {
                token: TokenType::Define,
                literal: "define".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Identifier,
                literal: "my_constant".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Hex,
                literal: "FE".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
                line_number: 1,
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
                literal: "LDA".to_string(),
                line_number: 2,
            },
            Token {
                token: TokenType::LiteralNumber,
                literal: "#".to_string(),
                line_number: 2,
            },
            Token {
                token: TokenType::Hex,
                literal: "00".to_string(),
                line_number: 2,
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
                line_number: 3,
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
                literal: "LDA".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::ParenLeft,
                literal: "(".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Hex,
                literal: "D2".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Comma,
                literal: ",".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Identifier,
                literal: "X".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::ParenRight,
                literal: ")".to_string(),
                line_number: 1,
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
                line_number: 1,
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
}
