use crate::*;

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Token {
    Identifier,
    Keyword(Keyword),
    Sigil(Sigil),
    BeginLiteral(Literal),
    EndLiteral(Literal),
    Comment,
    NewLine,
    Whitespace,
    EOF,
    Invalid(Invalid),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            Identifier => write!(f, "Identifier"),
            Keyword(keyword) => write!(f, "{}", keyword),
            Sigil(sigil) => write!(f, "{}", sigil),
            BeginLiteral(lit) => write!(f, "BeginLiteral({})", lit),
            EndLiteral(lit) => write!(f, "EndLiteral({})", lit),
            Comment => write!(f, "Comment"),
            NewLine => write!(f, "NewLine"),
            Whitespace => write!(f, "Whitespace"),
            EOF => write!(f, "EOF"),
            Invalid(invalid) => write!(f, "{:?}", invalid),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Invalid {
    UnterminatedComment,
    UnterminatedString,
    UnknownToken,
}
