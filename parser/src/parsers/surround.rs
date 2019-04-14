use super::*;
use std::fmt::Debug;


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
    S: SurroundPair<'a>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        self.1.open_syntax().test(parser)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut self.1.open_syntax())?;
        let res = parser.expect(&mut self.0)?;
        parser.expect(&mut self.1.close_syntax())?;
        Ok(res)
    }
}

pub trait SurroundPair<'a>: Debug {
    type Open: Syntax<'a>;
    type Close: Syntax<'a>;

    fn open_syntax(&mut self) -> &mut Self::Open;
    fn close_syntax(&mut self) -> &mut Self::Close;
}

#[derive(Debug)]
pub struct Pair<O, C>(pub O, pub C);

impl<O, C> Pair<O, C> {
    pub fn new(open: O, close: C) -> Self {
        Self(open, close)
    }
}

impl<'a, O, C> SurroundPair<'a> for Pair<O, C>
where
    O: Syntax<'a>,
    C: Syntax<'a>,
{
    type Open = O;
    type Close = C;

    fn open_syntax(&mut self) -> &mut Self::Open {
        &mut self.0
    }
    fn close_syntax(&mut self) -> &mut Self::Close {
        &mut self.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn surround() {
        let filename = diag::FileName::new("surround");
        let input: diag::Text = "{ foo } {foo}".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Surround::new(
            Token::Identifier,
            Pair::new(Sigil::OpenBrace, Sigil::CloseBrace),
        );

        let mut parser = crate::Parser::new(filename, &input, &tokens);
        let ok = parser.expect(&mut syntax).unwrap();
        assert_eq!(ok.value, Token::Identifier);
        assert_eq!(parser.string_at(ok.span).unwrap(), "foo");

        let ok = parser.expect(&mut syntax).unwrap();
        assert_eq!(ok.value, Token::Identifier);
        assert_eq!(parser.string_at(ok.span).unwrap(), "foo");
    }
}
