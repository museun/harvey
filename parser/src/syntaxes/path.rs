use super::*;

#[derive(Debug)]
pub struct Path;

impl<'a> Syntax<'a> for Path {
    type Output = hir::Path;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier) || parser.is(Sigil::Underscore)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if !parser.is(Token::Identifier) && !parser.is(Sigil::Underscore) {
            return Err(parser.report_error(parser.current_span(), "expected identifier or _"));
        }

        if parser.is(Token::Identifier) {
            return parser.expect(&mut Identifier).map(hir::Path::Ident);
        }
        parser
            .expect(&mut Sigil::Underscore)
            .map(|_| hir::Path::Glob)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn path() {
        let filename = diag::FileName::new("path");
        let inputs = &[
            ("foo", Some("foo")),
            ("foo.bar", Some("foo")),
            ("foo._", Some("foo")),
            ("_", None),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

            let parts = Parser::new(filename, &input, &tokens)
                .expect(&mut Path)
                .unwrap();

            assert_eq!(
                parts,
                match expected {
                    Some(s) => hir::Path::Ident(hir::Identifier {
                        name: s.to_string()
                    }),
                    None => hir::Path::Glob,
                }
            );
        }
    }
}
