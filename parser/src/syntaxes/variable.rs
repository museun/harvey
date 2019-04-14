use super::*;

/// Parse an identifier
#[derive(Debug)]
pub struct Identifier;
impl<'a> Syntax<'a> for Identifier {
    type Output = hir::Identifier;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if !self.test(parser) {
            return Err(parser.report_error(parser.current_span(), "expected a variable"));
        }

        let diag::Spanned { span, .. } = parser.shift();
        parser
            .string_at(span)
            .map(|name| hir::Identifier {
                name: name.to_string(),
            })
            .ok_or_else(|| parser.report_error(span, "expected a variable"))
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
            .expect(&mut Identifier)
            .unwrap();
        assert_eq!(var.name, "foo");

        let input: diag::Text = "let".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        Parser::new(filename, &input, &tokens)
            .expect(&mut Identifier)
            .unwrap_err();
    }
}
