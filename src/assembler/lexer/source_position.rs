use std::fmt;

/// Represents a position in the source code.
///
/// The line and column are 1-based.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
}

impl SourcePosition {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn increment_column(&mut self) {
        self.column += 1;
    }

    pub fn increment_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }
}

impl Default for SourcePosition {
    fn default() -> Self {
        Self::new(1, 1)
    }
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.line, self.column)
    }
}

/// Represents a span of positions in the source code.
///
/// The start position is inclusive, while the end position is exclusive.
/// Empty spans are allowed, where the start and end positions are the same.
#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub struct SourcePositionSpan {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

impl SourcePositionSpan {
    pub fn new(start: SourcePosition, end: SourcePosition) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for SourcePositionSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
pub struct TextSpan<'a> {
    pub text: &'a str,
    pub span: SourcePositionSpan,
}

impl<'a> TextSpan<'a> {
    pub fn new(text: &'a str, span: SourcePositionSpan) -> Self {
        Self { text, span }
    }
}
