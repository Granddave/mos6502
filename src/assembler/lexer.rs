#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// `#` Literal number prefix
    LiteralNumber,
    /// `$` Hex prefix
    Hex,
    /// `:` Label suffix
    Colon,
    /// `,`
    Comma,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// Instruction mnemonic or label
    Identifier,
    /// Marks the end of file
    Eof,
}

impl Default for TokenType {
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
    pub fn new() -> Self {
        Self {
            token: TokenType::Eof,
            literal: "".to_string(),
            line_number: 1,
        }
    }
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

pub struct Lexer<'a> {
    /// Source code to lex
    input: &'a str,
    /// Index of current position in input (points to current char)
    position: usize,
    /// Index of current reading position in input (after current char)
    read_position: usize,
    /// Current char under examination
    ch: Option<char>,
    /// Current line number in source code
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            line_number: 1,
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
            if self.ch.unwrap() == '\n' {
                self.line_number += 1;
            }
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
        while self.ch.is_some() && self.ch.unwrap().is_ascii_hexdigit() {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn create_token(&mut self, token: TokenType, literal: &str) -> Token {
        Token {
            token,
            literal: literal.to_string(),
            line_number: self.line_number,
        }
    }

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
                    let identifier = self.read_identifier();
                    Some(self.create_token(TokenType::Identifier, &identifier))
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
