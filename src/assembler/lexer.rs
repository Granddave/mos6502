// Example code:
//
//   LDX #$00
//   LDY #$00
// firstloop:
//   TXA
//   STA $0200,Y
//   PHA
//   INX
//   INY
//   CPY #$10
//   BNE firstloop ;loop until Y is $10
// secondloop:
//   PLA
//   STA $0200,Y
//   INY
//   CPY #$20      ;loop until Y is $20
//   BNE secondloop

// ':' = label, e.g. `my_label:`
// '$' = hex number, e.g. `$12`
// '#' = literal hex number, e.g. `#$12`
// ';' = comment, e.g. `; this is a comment`
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LiteralNumber, // '#'
    Hex,           // '$'
    Colon,         // ':'
    Comma,         // ','
    ParenLeft,     // '('
    ParenRight,    // ')'
    Identifier,
    Eof,
}

impl Default for TokenType {
    fn default() -> Self {
        Self::Eof
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Token {
    pub token: TokenType,
    pub literal: String,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self.token {
            TokenType::LiteralNumber => self.literal.to_owned(),
            TokenType::Hex => "$".to_string() + &self.literal,
            TokenType::Colon => ":".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::ParenLeft => "(".to_owned(),
            TokenType::ParenRight => ")".to_owned(),
            TokenType::Identifier => self.literal.to_owned(),
            TokenType::Eof => "<eof>".to_owned(),
        }
    }
}
impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token: token_type,
            literal,
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,       // Input string
    position: usize,      // Current position in input (points to current char)
    read_position: usize, // Current reading position in input (after current char)
    ch: Option<char>,     // Current char under examination
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.chars().nth(self.read_position).unwrap());
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.chars().nth(self.read_position).unwrap())
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some() && self.ch.unwrap().is_whitespace() {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.ch.is_some() && self.ch.unwrap() != '\n' {
            self.read_char();
        }
    }

    /// Instruction mnemonic or label
    fn read_identifier(&mut self) -> String {
        let position = self.position;
        // Allow alphanumeric and underscore
        while self.ch.is_some() && (self.ch.unwrap().is_alphanumeric() || self.ch.unwrap() == '_') {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_hex(&mut self) -> String {
        let position = self.position;
        while self.ch.is_some() && self.ch.unwrap().is_digit(16) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token = match self.ch {
            Some(ch) => match ch {
                ';' => {
                    self.skip_comment();
                    self.next_token()
                }
                '$' => {
                    self.read_char();
                    let token = Token {
                        token: TokenType::Hex,
                        literal: self.read_hex(),
                    };
                    Some(token)
                }
                '#' => {
                    self.read_char();
                    Some(Token {
                        token: TokenType::LiteralNumber,
                        literal: '#'.to_string(),
                    })
                }
                ':' => {
                    self.read_char();
                    Some(Token {
                        token: TokenType::Colon,
                        literal: ':'.to_string(),
                    })
                }
                ',' => {
                    self.read_char();
                    Some(Token {
                        token: TokenType::Comma,
                        literal: ','.to_string(),
                    })
                }
                '(' => {
                    self.read_char();
                    Some(Token {
                        token: TokenType::ParenLeft,
                        literal: '('.to_string(),
                    })
                }
                ')' => {
                    self.read_char();
                    Some(Token {
                        token: TokenType::ParenRight,
                        literal: ')'.to_string(),
                    })
                }
                'A'..='Z' | 'a'..='z' | '_' => Some(Token {
                    token: TokenType::Identifier,
                    literal: self.read_identifier(),
                }),
                _ => {
                    panic!("Lexer: Unexpected character: '{}'", ch)
                }
            },
            None => Some(Token {
                token: TokenType::Eof,
                literal: "".to_string(),
            }),
        };

        // eprintln!("Lexer: {:?}", token);
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_peek_char() {
        let input = "LDA";
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.peek_char(), Some('D'));
        assert_eq!(lexer.peek_char(), Some('D'));
        lexer.read_char();
        assert_eq!(lexer.peek_char(), Some('A'));
    }

    #[test]
    fn test_hex() {
        let tests = vec![
            ("$00", "00"),
            ("$01", "01"),
            ("$10", "10"),
            ("$ff", "ff"),
            ("$FF", "FF"),
        ];
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input);
            let token = lexer.next_token().unwrap();
            assert_eq!(token.token, TokenType::Hex);
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
        let input = "LDA #$00";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                literal: "LDA".to_string(),
            },
            Token {
                token: TokenType::LiteralNumber,
                literal: "#".to_string(),
            },
            Token {
                token: TokenType::Hex,
                literal: "00".to_string(),
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
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
    fn test_label() {
        let input = "my_label:";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                literal: "my_label".to_string(),
            },
            Token {
                token: TokenType::Colon,
                literal: ":".to_string(),
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
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
        let input = "; this is a comment\nLDA #$00 ; Another one\n;Last one";
        let result = vec![
            Token {
                token: TokenType::Identifier,
                literal: "LDA".to_string(),
            },
            Token {
                token: TokenType::LiteralNumber,
                literal: "#".to_string(),
            },
            Token {
                token: TokenType::Hex,
                literal: "00".to_string(),
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
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
            },
            Token {
                token: TokenType::ParenLeft,
                literal: "(".to_string(),
            },
            Token {
                token: TokenType::Hex,
                literal: "D2".to_string(),
            },
            Token {
                token: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token: TokenType::Identifier,
                literal: "X".to_string(),
            },
            Token {
                token: TokenType::ParenRight,
                literal: ")".to_string(),
            },
            Token {
                token: TokenType::Eof,
                literal: "".to_string(),
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
