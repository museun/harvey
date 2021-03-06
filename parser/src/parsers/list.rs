use super::*;

#[derive(Debug)]
pub struct List<T, D> {
    pub element: T,
    pub delimiter: D,
}

impl<T, D> List<T, D> {
    pub fn new(element: T, delimiter: D) -> Self {
        Self { element, delimiter }
    }
}

impl<'a, T, D> Syntax<'a> for List<T, D>
where
    T: Syntax<'a>,
    D: Syntax<'a>,
{
    type Output = Vec<T::Output>;

    fn test(&mut self, _parser: &Parser<'a>) -> bool {
        true // we will never produce empty lists
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let mut output = vec![];
        while parser.test(&mut self.element) {
            output.push(parser.expect(&mut self.element)?);
            if !parser.test(&mut self.delimiter) {
                break;
            }
            parser.expect(&mut self.delimiter)?;
        }
        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn list() {
        let filename = diag::FileName::new("list");

        let input: diag::Text = "A, B, C, D, E,a,b,c,d,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = List::new(
            Token::Identifier,
            Then(Sigil::Comma, Sink(Token::Whitespace)),
        );
        let res = Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap()
            .pop()
            .unwrap();

        assert_eq!(res.len(), 9);
        assert!(res.iter().all(|k| k.value == Token::Identifier));

        let input: diag::Text = "A, B, C, D,\na,b,c,d,e,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let list = Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap();
        assert_eq!(list.len(), 2);

        assert_eq!(list[0].len(), 4);
        assert!(list[0].iter().all(|k| k.value == Token::Identifier));

        assert_eq!(list[1].len(), 5);
        assert!(list[1].iter().all(|k| k.value == Token::Identifier));
    }
}
