use super::source_position::SourcePositionSpan;

/// TokenType definies the types of tokens that are found in source code.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // --- Single character tokens ---
    /// Dot
    Dot,
    /// `:` Label suffix character
    Colon,
    /// `,`
    Comma,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// `#` Literal number prefix character
    Pound,

    // --- Literals ---
    /// `$` Hex prefix including hex number
    HexNumber,
    /// `%` Binary prefix including binary number
    BinaryNumber,
    /// Decimal number (no prefix)
    DecimalNumber,
    /// Instruction mnemonic, label or constant definition.
    ///
    /// Basically anything that starts with a letter or underscore.
    Identifier,

    // --- Keywords ---
    /// Constant definition keyword
    Define,
    /// Org directive keyword
    OrgDirective,

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
    pub lexeme: String,
    /// Line number in file where token is found
    pub span: SourcePositionSpan,
}

impl Token {
    #[tracing::instrument]
    pub fn new(token: TokenType, lexeme: &str, span: SourcePositionSpan) -> Self {
        Self {
            token,
            lexeme: lexeme.to_owned(),
            span,
        }
    }

    #[tracing::instrument]
    fn literal_str(&self) -> String {
        match self.token {
            TokenType::Dot => ".".to_owned(),
            TokenType::Colon => ":".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::ParenLeft => "(".to_owned(),
            TokenType::ParenRight => ")".to_owned(),
            TokenType::Pound => "#".to_owned(),
            TokenType::HexNumber => "$".to_string() + &self.lexeme,
            TokenType::BinaryNumber => "%".to_string() + &self.lexeme,
            TokenType::DecimalNumber => self.lexeme.to_owned(),
            TokenType::Identifier => self.lexeme.to_owned(),
            TokenType::Define => "define".to_owned(),
            TokenType::OrgDirective => "org".to_owned(),
            TokenType::Eof => "<eof>".to_owned(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {:?} '{}'",
            self.span,
            self.token,
            self.literal_str()
        )
    }
}
