/// TokenType definies the types of tokens that are found in source code.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// `#` Literal number prefix character
    LiteralNumber,
    /// `$` Hex prefix including hex number
    Hex,
    /// Decimal number
    Decimal,
    /// `:` Label suffix character
    Colon,
    /// Constant definition keyword
    Define,
    /// `,`
    Comma,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// Instruction mnemonic, label or constant definition.
    ///
    /// Basically anything that starts with a letter or underscore.
    Identifier,
    /// Eof marks the end of file
    Eof,
}

impl Default for TokenType {
    #[tracing::instrument]
    fn default() -> Self {
        Self::Eof
    }
}

/// Token is a lexical unit of source code.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Token {
    /// Type of Token
    pub token: TokenType,
    /// Literal string of token, e.g. `"LDA"`, `"00"`, `":"` etc.
    pub literal: String,
    /// Line number in file where token is found
    pub line_number: usize,
}

impl Token {
    #[tracing::instrument]
    pub fn new() -> Self {
        Self {
            token: TokenType::Eof,
            literal: "".to_string(),
            line_number: 1,
        }
    }
}

impl ToString for Token {
    #[tracing::instrument]
    fn to_string(&self) -> String {
        match self.token {
            TokenType::LiteralNumber => self.literal.to_owned(),
            TokenType::Hex => "$".to_string() + &self.literal,
            TokenType::Decimal => self.literal.to_owned(),
            TokenType::Colon => ":".to_owned(),
            TokenType::Define => "define".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::ParenLeft => "(".to_owned(),
            TokenType::ParenRight => ")".to_owned(),
            TokenType::Identifier => self.literal.to_owned(),
            TokenType::Eof => "<eof>".to_owned(),
        }
    }
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
            self.ch = Some(self.src.chars().nth(self.read_position).unwrap());
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    #[tracing::instrument]
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        while self.ch.is_some() && self.ch.unwrap().is_whitespace() {
            if self.ch.unwrap() == '\n' {
                self.line_number += 1;
            }
            self.read_char();
        }
    }

    #[tracing::instrument]
    #[inline(always)]
    fn skip_comment(&mut self) {
        while self.ch.is_some() && self.ch.unwrap() != '\n' {
            self.read_char();
        }
    }

    /// Instruction mnemonic, label or constant definition
    #[tracing::instrument]
    fn read_string(&mut self) -> String {
        let position = self.position;
        // Allow alphanumeric and underscore
        while self.ch.is_some() && (self.ch.unwrap().is_alphanumeric() || self.ch.unwrap() == '_') {
            self.read_char();
        }
        self.src[position..self.position].to_string()
    }

    #[tracing::instrument]
    fn read_hex(&mut self) -> String {
        let position = self.position;
        while self.ch.is_some() && self.ch.unwrap().is_ascii_hexdigit() {
            self.read_char();
        }
        self.src[position..self.position].to_string()
    }

    #[tracing::instrument]
    fn create_token(&mut self, token: TokenType, literal: &str) -> Token {
        Token {
            token,
            literal: literal.to_string(),
            line_number: self.line_number,
        }
    }

    #[tracing::instrument]
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        match self.ch {
            Some(ch) => match ch {
                ';' => {
                    self.skip_comment();
                    self.next_token()
                }
                '$' => {
                    self.read_char();
                    let hex = self.read_hex();
                    Some(self.create_token(TokenType::Hex, &hex))
                }
                '0'..='9' => {
                    let decimal = self.read_string();
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
                    panic!("Lexer: Unexpected character: '{}'", ch)
                }
            },
            None => Some(self.create_token(TokenType::Eof, "")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex() {
        let tests = vec![
            ("$00", "00"),
            ("$01", "01"),
            ("$10", "10"),
            ("$ff", "ff"),
            ("$FF", "FF"),
            ("$FFFF", "FFFF"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token().unwrap();
            assert_eq!(token.token, TokenType::Hex);
            assert_eq!(token.literal, expected);
        }
    }

    #[test]
    fn test_decimal() {
        let tests = vec![
            ("0", "0"),
            ("1", "1"),
            ("10", "10"),
            ("123", "123"),
            ("65535", "65535"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token().unwrap();
            assert_eq!(token.token, TokenType::Decimal);
            assert_eq!(token.literal, expected);
        }
    }

    #[test]
    fn test_identifier() {
        let tests = vec![("LDA", "LDA"), ("X", "X"), ("my_label", "my_label")];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token().unwrap();
            assert_eq!(token.token, TokenType::Identifier);
            assert_eq!(token.literal, expected);
        }
    }

    #[test]
    fn test_instruction() {
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
                let token = lexer.next_token().unwrap();
                tokens.push(token.clone());
                if token.token == TokenType::Eof {
                    break;
                }
            }
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn test_label() {
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
            let token = lexer.next_token().unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
    }

    #[test]
    fn test_define() {
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
            let token = lexer.next_token().unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
    }

    #[test]
    fn test_comment() {
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
            let token = lexer.next_token().unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
    }

    #[test]
    fn test_paren() {
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
            let token = lexer.next_token().unwrap();
            tokens.push(token.clone());
            if token.token == TokenType::Eof {
                break;
            }
        }
        assert_eq!(tokens, result);
    }
}
