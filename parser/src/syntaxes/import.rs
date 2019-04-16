use super::*;

#[derive(Debug)]
pub struct Import;

impl<'a> Syntax<'a> for Import {
    type Output = hir::Import;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Keyword::Import)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Keyword::Import)?;

        let span = parser.current_span();
        let mut syntax = Path;

        let head = parser.expect(&mut syntax)?;
        if let hir::Path::Glob = head {
            return Err(parser.report_error_current("invalid import: glob cannot be used there"));
        }

        let mut paths = vec![];
        let mut path = vec![head];
        while !parser.is(Token::NewLine) {
            if parser.test(&mut Sigil::Comma) {
                parser.shift();
                let head = parser.expect(&mut syntax)?;
                if let hir::Path::Glob = head {
                    return Err(
                        parser.report_error_current("invalid import: glob cannot be used there")
                    );
                }
                paths.push(std::mem::replace(&mut path, vec![head]));
                continue;
            }

            if !parser.is(Sigil::Dot) {
                break;
            }
            parser.expect(&mut Sigil::Dot)?;
            path.push(parser.expect(&mut syntax)?);
        }
        if !path.is_empty() {
            paths.push(path)
        }
        if paths.is_empty() || paths.iter().any(Vec::is_empty) {
            return Err(parser.report_error(span, "empty import"));
        }
        Ok(hir::Import { paths })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn import() {
        let filename = diag::FileName::new("import");

        let inputs = &[
            ("import foo", vec![vec![Some("foo")]]),
            ("import foo.bar", vec![vec![Some("foo"), Some("bar")]]),
            ("import foo._", vec![vec![Some("foo"), None]]),
            (
                "import foo, bar, baz._",
                vec![
                    vec![Some("foo")],
                    vec![Some("bar")],
                    vec![Some("baz"), None],
                ],
            ),
            (
                "import foo._, bar",
                vec![vec![Some("foo"), None], vec![Some("bar")]],
            ),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

            Parser::new(filename, &input, &tokens)
                .expect(&mut Import)
                .unwrap()
                .paths
                .iter()
                .zip(expected.iter())
                .flat_map(|(l, r)| {
                    l.iter().zip(r.iter().map(|e| match e {
                        Some(s) => hir::Path::Ident(hir::Identifier {
                            name: s.to_string(),
                        }),
                        None => hir::Path::Glob,
                    }))
                })
                .for_each(|(test, expected)| assert_eq!(*test, expected))
        }

        let bad_inputs = &[
            "import",
            "import _",
            "import _.foo",
            "import foo, _",
            "import foo, _.bar",
        ];

        for bad in bad_inputs {
            let input: diag::Text = (*bad).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            Parser::new(filename, &input, &tokens)
                .expect(&mut Import)
                .unwrap_err();
        }
    }
}
