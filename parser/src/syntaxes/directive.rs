use super::*;

#[derive(Debug)]
pub struct Directive;
impl<'a> Syntax<'a> for Directive {
    type Output = hir::Directive;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::Hash)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Sigil::Hash)?;
        parser
            .expect(&mut Surround(
                Sigil::OpenBracket,
                Identifier,
                Sigil::CloseBracket,
            ))
            .map(|directive| hir::Directive { directive })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn directive() {
        let filename = diag::FileName::new("directive");
        let input: diag::Text = "#[export]".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        let mut parser = Parser::new(filename, &input, &tokens);
        assert_eq!(
            parser.expect(&mut Directive).unwrap(),
            hir::Directive {
                directive: hir::Identifier {
                    name: "export".into()
                }
            }
        );
    }
}
