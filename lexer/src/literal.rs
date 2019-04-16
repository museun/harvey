#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Literal {
    String,
    Hexadecimal,
    Integer,
    Octal,
    Binary,
    Float,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String => write!(f, "String"),
            Literal::Hexadecimal => write!(f, "Hexadecimal"),
            Literal::Integer => write!(f, "Integer"),
            Literal::Octal => write!(f, "Octal"),
            Literal::Binary => write!(f, "Binary"),
            Literal::Float => write!(f, "Float"),
        }
    }
}

impl PartialEq<crate::Token> for Literal {
    fn eq(&self, other: &crate::Token) -> bool {
        match other {
            crate::Token::BeginLiteral(lit) | crate::Token::EndLiteral(lit) => self.eq(lit),
            _ => false,
        }
    }
}
