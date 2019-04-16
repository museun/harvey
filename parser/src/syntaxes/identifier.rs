use super::*;

#[derive(Debug)]
pub struct Identifier;

impl<'a> Syntax<'a> for Identifier {
    type Output = hir::Identifier;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Token::Identifier)?;
        Ok(hir::Identifier {
            name: parser.current_str().to_string(),
        })
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
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .expect(&mut Identifier)
                .unwrap()
                .name,
            "foo"
        );

        let input: diag::Text = "   foo    bar   baz ".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut Identifier)
                .unwrap(),
            vec![
                hir::Identifier { name: "foo".into() },
                hir::Identifier { name: "bar".into() },
                hir::Identifier { name: "baz".into() }
            ]
        );

        let input: diag::Text = "let".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        Parser::new(filename, &input, &tokens)
            .expect(&mut Identifier)
            .unwrap_err();
    }
}
