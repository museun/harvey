use super::*;

#[derive(Debug)]
pub struct Identifier;

impl<'a> Syntax<'a> for Identifier {
    type Output = hir::Identifier;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if !self.test(parser) {
            return Err(parser.report_error(parser.current_span(), "expected an identifier"));
        }

        let diag::Spanned { span, .. } = parser.shift();
        parser
            .string_at(span)
            .map(|name| hir::Identifier {
                name: name.to_string(),
            })
            .ok_or_else(|| parser.report_error(span, "expected an identifier"))
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn identifier() {
        let filename = diag::FileName::new("identifier");

        let input: diag::Text = "foo".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        let identifier = Parser::new(filename, &input, &tokens)
            .expect(&mut Identifier)
            .unwrap();
        assert_eq!(identifier.name, "foo");

        let input: diag::Text = "let".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        Parser::new(filename, &input, &tokens)
            .expect(&mut Identifier)
            .unwrap_err();
    }
}
