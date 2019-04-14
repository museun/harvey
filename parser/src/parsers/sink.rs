use super::*;

#[derive(Debug)]
pub struct Sink<T>(pub T);

impl<T> Sink<T> {
    pub fn new(t: T) -> Self {
        Self(t)
    }
}

impl<'a, T> Syntax<'a> for Sink<T>
where
    T: Syntax<'a>,
{
    type Output = usize;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        self.0.test(parser)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let mut count = 0;
        while parser.test(&mut self.0) {
            parser.shift();
            count += 1;
        }
        Ok(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn sink() {
        let filename = diag::FileName::new("sink");

        let input: diag::Text = "1234567890".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(parser.expect(&mut Sink(Token::Integer)).unwrap(), 10);
        assert!(parser.is(Token::EOF));
    }
}
