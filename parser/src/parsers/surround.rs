use super::*;
use std::fmt::Debug;

pub trait Surrounder<'a>: Debug {
    type Open: Syntax<'a>;
    type Close: Syntax<'a>;

    fn open_syntax(&self) -> Self::Open;
    fn close_syntax(&self) -> Self::Close;
}

#[derive(Debug)]
pub struct Surround<T, S>(pub T, pub S);

impl<T, S> Surround<T, S> {
    pub fn new(data: T, surround: S) -> Self {
        Self(data, surround)
    }
}

impl<'a, T, S> Syntax<'a> for Surround<T, S>
where
    T: Syntax<'a>,
    S: Surrounder<'a>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        unimplemented!()
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        unimplemented!()
    }
}

/*
struct Parens {}
impl<'a> Surrounder<'a> for Parens {
    type Open = super::Token(Sigil::OpenParen);
    type Close = super::Token(Sigil::CloseParen);

    fn open_syntax(&self) -> Self::Open {
        unimplemented!()
    }
    fn close_syntax(&self) -> Self::Close {
        unimplemented!()
    }
}

struct Braces {}
impl<'a> Surrounder<'a> for Braces {
    type Open = super::Token(Sigil::OpenBrace);
    type Close = super::Token(Sigil::CloseBrace);

    fn open_syntax(&self) -> Self::Open {
        unimplemented!()
    }
    fn close_syntax(&self) -> Self::Close {
        unimplemented!()
    }
}

struct Brackets {}
impl<'a> Surrounder<'a> for Brackets {
    type Open = super::Token(Sigil::OpenBracket);
    type Close = super::Token(Sigil::CloseBracket);

    fn open_syntax(&self) -> Self::Open {
        unimplemented!()
    }
    fn close_syntax(&self) -> Self::Close {
        unimplemented!()
    }
}

struct Ticks {}
impl<'a> Surrounder<'a> for Ticks {
    type Open = super::Token(Sigil::Tick);
    type Close = super::Token(Sigil::Tick);

    fn open_syntax(&self) -> Self::Open {
        unimplemented!()
    }
    fn close_syntax(&self) -> Self::Close {
        unimplemented!()
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn surround() {}
}
