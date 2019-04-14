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

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        true // we will never produce empty lists
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let mut output = vec![];
        while self.element.test(parser) {
            output.push(self.element.expect(parser)?);
            if !self.delimiter.test(parser) {
                break;
            }
            parser.shift();
        }
        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list() {
        let filename = diag::FileName::new("delimited");

        let input: diag::Text = "1, 2, 3, 4, 5,6,7,8,9,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax = List::new(Token::Integer, Sigil::Comma);
        let res = crate::Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap()
            .pop()
            .unwrap();

        assert_eq!(res.len(), 9);
        assert!(res.iter().all(|k| k.value == Token::Integer));

        let input: diag::Text = "1, 2, 3, 4,\n5,6,7,8,9,".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let list = crate::Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap();
        assert_eq!(list.len(), 2);

        assert_eq!(list[0].len(), 4);
        assert!(list[0].iter().all(|k| k.value == Token::Integer));

        assert_eq!(list[1].len(), 5);
        assert!(list[1].iter().all(|k| k.value == Token::Integer));
    }
}
