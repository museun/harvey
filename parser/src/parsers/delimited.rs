use super::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Delimited<T, D> {
    pub element: T,
    pub delim: D,
    optional: bool,
}

impl<'a, T, D> Delimited<T, D> {
    pub fn new(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: false,
        }
    }
    pub fn optional(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: true,
        }
    }
}

impl<'a, T, D> Syntax<'a> for Delimited<T, D>
where
    T: Syntax<'a>,
    D: Syntax<'a>,
{
    type Output = T::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut self.element)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Self {
            element,
            delim,
            optional,
        } = self;
        let el = parser.expect(element)?;
        if *optional {
            if !parser.test(delim) {
                return Ok(el);
            } else {
                parser.shift();
            }
        }
        parser.expect(delim).map(|_| el)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn delimited() {
        let filename = diag::FileName::new("delimited");

        let input: diag::Text = "1,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Delimited::new(Token::Integer, Sigil::Comma);
        let mut parser = crate::Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.parse_until_eof(&mut syntax).unwrap()[0].value,
            Token::Integer
        );

        let input: diag::Text = "1".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Delimited::optional(Token::Integer, Sigil::Comma);
        let mut parser = crate::Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.parse_until_eof(&mut syntax).unwrap()[0].value,
            Token::Integer
        );
    }
}
