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
    pub fn new(token: TokenType, literal: &str, line_number: usize) -> Self {
        Self {
            token,
            literal: literal.to_owned(),
            line_number,
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
