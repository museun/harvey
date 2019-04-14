use super::*;
use std::fmt::Debug;


#[derive(Debug)]
pub struct Surround<Open, Element, Close>(pub Open, pub Element, pub Close);

impl<Open, Element, Close> Surround<Open, Element, Close> {
    pub fn new(open: Open, element: Element, close: Close) -> Self {
        Self(open, element, close)
    }
}

impl<'a, Open, Element, Close> Syntax<'a> for Surround<Open, Element, Close>
where
    Open: Syntax<'a>,
    Element: Syntax<'a>,
    Close: Syntax<'a>,
{
    type Output = Element::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        self.0.test(parser)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Surround(open, el, close) = self;
        parser.expect(open)?;
        let res = parser.expect(el)?;
        parser.expect(close)?;
        Ok(res)
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
            Sigil::OpenBrace, //
            Token::Identifier,
            Sigil::CloseBrace,
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
