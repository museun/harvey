use super::*;

#[derive(Debug)]
pub struct Variable;
impl<'a> Syntax<'a> for Variable {
    type Output = hir::Variable;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Token::Identifier)?;

        Ok(hir::Variable {
            name: parser.current_str().to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variable() {
        let filename = diag::FileName::new("variable");

        let input: diag::Text = "foo".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        let var = Parser::new(filename, &input, &tokens)
            .expect(&mut Variable)
            .unwrap();
        assert_eq!(var.name, "foo");

        let input: diag::Text = "let".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        Parser::new(filename, &input, &tokens)
            .expect(&mut Variable)
            .unwrap_err();
    }
}
