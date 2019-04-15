use super::*;

/// Parse an identifier
#[derive(Debug)]
pub struct Variable;
impl<'a> Syntax<'a> for Variable {
    type Output = hir::Variable;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if !self.test(parser) {
            return Err(parser.report_error_current("expected a variable"));
        }

        let diag::Spanned { span, .. } = parser.shift();
        Ok(hir::Variable {
            name: parser.string(span).to_string(),
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
