use crate::*;

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Token {
    Identifier,
    Keyword(Keyword),
    Sigil(Sigil),
    Primitive(Primitive),
    String,
    Integer,
    Comment,
    NewLine,
    Whitespace,
    EOF,
    Invalid(Invalid),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Invalid {
    UnterminatedComment,
    UnterminatedString,
    UnknownToken,
}
