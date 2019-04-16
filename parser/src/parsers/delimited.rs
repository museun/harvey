use super::*;

#[derive(Debug)]
pub struct Delimited<T, D> {
    pub element: T,
    pub delim: D,
    optional: bool,
    many: bool,
}

impl<'a, T, D> Delimited<T, D> {
    pub fn new(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: false,
            many: false,
        }
    }
    pub fn optional(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: true,
            many: false,
        }
    }
    pub fn many(element: T, delim: D) -> Self {
        Self {
            element,
            delim,
            optional: true,
            many: true,
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
            many,
        } = self;
        let el = parser.expect(element)?;
        if *optional {
            while parser.test(delim) {
                parser.shift();
                if !*many {
                    break;
                }
            }
            return Ok(el);
        }
        parser.expect(delim).map(|_| el)
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn delimited() {
        let filename = diag::FileName::new("delimited");
        let input: diag::Text = "A,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Delimited::new(Token::Identifier, Sigil::Comma);
        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.parse_until_eof(&mut syntax).unwrap()[0].value,
            Token::Identifier
        );
    }

    #[test]
    fn optional() {
        let filename = diag::FileName::new("delim_optional");
        let input: diag::Text = "A".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Delimited::optional(Token::Identifier, Sigil::Comma);
        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.parse_until_eof(&mut syntax).unwrap()[0].value,
            Token::Identifier
        );
    }

    #[test]
    fn many() {
        let filename = diag::FileName::new("delim_optional");
        let input: diag::Text = ".____._..".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = Delimited::many(Sigil::Dot, Sigil::Underscore);
        let mut parser = Parser::new(filename, &input, &tokens);
        let list = parser.parse_until_eof(&mut syntax).unwrap();
        assert_eq!(list.len(), 4);
        assert!(list.iter().all(|&k| k.value == Sigil::Dot));
    }
}
