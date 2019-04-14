use super::*;

pub trait UnitToken {
    fn unit() -> Token;
}

impl UnitToken for Token {
    fn unit() -> Token {
        Token::EOF
    }
}

impl UnitToken for Sigil {
    fn unit() -> Token {
        Sigil::Unit.into()
    }
}

impl UnitToken for Keyword {
    fn unit() -> Token {
        Keyword::Let.into()
    }
}
