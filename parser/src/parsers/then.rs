use super::*;

#[derive(Debug)]
pub struct Then<L, R>(pub L, pub R);
impl<L, R> Then<L, R> {
    pub fn new(this: L, then: R) -> Self {
        Self(this, then)
    }
}

impl<'a, L, R> Syntax<'a> for Then<L, R>
where
    L: Syntax<'a>,
    R: Syntax<'a>,
{
    type Output = L::Output;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut self.0)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let out = parser.expect(&mut self.0)?;
        if parser.test(&mut self.1) {
            parser.expect(&mut self.1)?;
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn then() {
        let mut syntax = Then::new(Token::Identifier, Sink::new(Token::Whitespace));

        let filename = diag::FileName::new("then");
        let input: diag::Text = "foo bar                    baz        ".into();
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap()
            .into_iter()
            .map(|k| &input[k.span])
            .zip(&["foo", "bar", "baz"])
            .for_each(|(test, expected)| assert_eq!(test, *expected));
    }
}
