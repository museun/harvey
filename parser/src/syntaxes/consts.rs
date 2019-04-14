use super::*;

#[derive(Debug)]
pub struct Const;
impl<'a> Syntax<'a> for Const {
    type Output = hir::Const;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        unimplemented!()
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn const_() {
        let filename = diag::FileName::new("const");
        let input: diag::Text = "const PI = 3.2".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.expect(&mut Const).unwrap(),
            hir::Const {
                identifier: hir::Identifier { name: "PI".into() },
                literal: hir::Literal::Float(3.2)
            }
        );
    }
}
