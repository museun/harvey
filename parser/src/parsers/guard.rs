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
    T: Syntax<'a>,
    E: Syntax<'a>,
{
    type Output = E::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        let Guard(guard, ..) = self;
        parser.test(guard)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Guard(guard, val) = self;
        parser.expect(guard)?;
        parser.expect(val)
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn guard() {
        let filename = diag::FileName::new("guard");

        let input: diag::Text = "foo -> bar".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut parser = Parser::new(filename, &input, &tokens);
        let tok = parser.expect(&mut Token::Identifier).unwrap();
        assert_eq!(tok.value, Token::Identifier);
        assert_eq!(parser.string(tok.span), "foo");

        let tok = parser
            .expect(&mut Guard::new(Sigil::Arrow, Token::Identifier))
            .unwrap();
        assert_eq!(tok.value, Token::Identifier);
        assert_eq!(parser.string(tok.span), "bar");
    }
}
