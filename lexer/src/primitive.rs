use phf_derive::PerfectHash;

#[derive(Debug, Copy, Clone, PartialEq, Hash, PerfectHash)]
pub enum Primitive {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

impl From<Primitive> for crate::Token {
    fn from(prim: Primitive) -> Self {
        crate::Token::Primitive(prim)
    }
}

impl PartialEq<crate::Token> for Primitive {
    fn eq(&self, other: &crate::Token) -> bool {
        if let crate::Token::Primitive(kw) = other {
            return self.eq(kw);
        }
        false
    }
}
