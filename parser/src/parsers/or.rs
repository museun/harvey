use super::*;

#[derive(Debug)]
pub struct Or<L, R>(pub L, pub R);

impl<L, R> Or<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self(left, right)
    }
}

impl<'a, L, R> Syntax<'a> for Or<L, R>
where
    L: Syntax<'a>,
    R: Syntax<'a, Output = L::Output>,
{
    type Output = L::Output;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        let Or(left, right) = self;
        parser.test(left) || parser.test(right)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let Or(left, right) = self;
        if parser.test(left) {
            return parser.expect(left);
        }
        parser.expect(right)
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn or() {
        let mut syntax = Or(Sigil::Comma, Sigil::Dot);

        let inputs = &[
            (",,", vec![Sigil::Comma, Sigil::Comma]),
            ("..", vec![Sigil::Dot, Sigil::Dot]),
            (".,", vec![Sigil::Dot, Sigil::Comma]),
            (",.", vec![Sigil::Comma, Sigil::Dot]),
        ];
        let file = diag::FileName::new("or");

        for (input, expected) in inputs {
            let source: diag::Text = (*input).into();
            let tokens = Lexer::new(&source, file).into_iter().collect::<Vec<_>>();
            let t = Parser::new(file, &source, &tokens)
                .parse_until_eof(&mut syntax)
                .unwrap();

            for (test, expected) in t.iter().zip(expected.iter()) {
                assert_eq!(test.value, *expected)
            }
        }
    }
}
