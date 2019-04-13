use super::*;

/// for parsing things like -> Foo. will drop the -> and return the Foo
#[derive(Debug)]
pub struct Guard<T, E>(pub T, pub E);

impl<'a, T, E> Guard<T, E> {
    pub fn new(t: T, e: E) -> Self {
        Self(t, e)
    }
}

impl<'a, T, E> Syntax<'a> for Guard<T, E>
where
    T: Syntax<'a, Output = E::Output>,
    E: Syntax<'a>,
{
    type Output = E::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut self.1)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Guard(guard, val) = self;
        parser.expect(guard)?;
        parser.expect(val)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn guard() {
        let mut syntax = Guard::new(
            lexer::Token::Sigil(lexer::Sigil::Arrow),
            lexer::Token::Identifier,
        );

        let input: diag::Text = "foo -> bar".into();
        let filename = diag::FileName::new("guard");

        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        let mut parser = crate::Parser::new(filename, &input, &tokens);

        let tok = parser.expect(&mut lexer::Token::Identifier).unwrap();
        assert_eq!(tok.value, lexer::Token::Identifier);
        assert_eq!(parser.string_at(tok.span).unwrap(), "foo");

        let tok = parser.expect(&mut syntax).unwrap();
        assert_eq!(tok.value, lexer::Token::Identifier);
        assert_eq!(parser.string_at(tok.span).unwrap(), "bar");
    }
}
