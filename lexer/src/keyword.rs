use phf_derive::PerfectHash;

#[derive(Debug, Copy, Clone, PartialEq, Hash, PerfectHash)]
pub enum Keyword {
    Import,
    Const,
    Let,
    Mut,
    Type,
    Enum,
    Match,
    When,
    Impl,
    Return,
    Break,
    Continue,
    For,
}

impl From<Keyword> for crate::Token {
    fn from(keyword: Keyword) -> Self {
        crate::Token::Keyword(keyword)
    }
}

impl PartialEq<crate::Token> for Keyword {
    fn eq(&self, other: &crate::Token) -> bool {
        if let crate::Token::Keyword(kw) = other {
            return self.eq(kw);
        }
        false
    }
}
