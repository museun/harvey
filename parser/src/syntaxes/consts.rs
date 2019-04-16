use super::*;

#[derive(Debug)]
pub struct Const;
impl<'a> Syntax<'a> for Const {
    type Output = hir::Const;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Keyword::Const)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let identifier = parser.expect(&mut Guard(Keyword::Const, Identifier))?;
        let literal = parser.expect(&mut Guard(Sigil::Equal, Literal))?;
        Ok(hir::Const {
            identifier,
            literal,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
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

        let input: diag::Text = "const NAME = \"obscure name can go here\"".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.expect(&mut Const).unwrap(),
            hir::Const {
                identifier: hir::Identifier {
                    name: "NAME".into()
                },
                literal: hir::Literal::String("obscure name can go here".into())
            }
        );
    }
}
