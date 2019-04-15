use super::*;
use diag::{FileName, Spanned};

impl<'a> Syntax<'a> for Token {
    type Output = Spanned<Self, FileName>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if !self.test(parser) {
            return Err(parser.report_error_next(format!("expected '{}'", self)));
        }

        let Spanned { value, span, .. } = parser.shift();
        Ok(Spanned::new(value, span))
    }
}

impl<'a> Syntax<'a> for Keyword {
    type Output = Spanned<Self, FileName>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if self.test(parser) {
            let tok = parser.shift();
            if let Token::Keyword(keyword) = tok.value {
                return Ok(Spanned::new(keyword, tok.span));
            }
            unreachable!("should have parsed a keyword")
        }
        Err(parser.report_error_current(format!("expected '{}'", self)))
    }
}

impl<'a> Syntax<'a> for Sigil {
    type Output = Spanned<Self, FileName>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if self.test(parser) {
            let tok = parser.shift();
            if let Token::Sigil(sigil) = tok.value {
                return Ok(Spanned::new(sigil, tok.span));
            }
            unreachable!("should have parsed a Sigil")
        }
        Err(parser.report_error_current(format!("expected '{}'", self)))
    }
}


#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn token() {
        let filename = FileName::new("lex_token");

        let input = "this is a test";
        let expected = input.split_whitespace();

        let input: diag::Text = input.into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut parser = Parser::new(filename, &input, &tokens);
        for (test, expected) in parser
            .parse_until_eof(&mut Token::Identifier)
            .unwrap()
            .iter()
            .zip(expected)
        {
            assert_eq!(parser.string(test.span), expected);
        }
    }

    #[test]
    fn sigil() {
        let filename = FileName::new("lex_sigil");

        let input: diag::Text = "    () ()       ()\n   ()()()".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        assert!(Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut Sigil::Unit)
            .unwrap()
            .iter()
            .all(|s| s.value == Sigil::Unit));

        let input: diag::Text = "->".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        dbg!(Parser::new(filename, &input, &tokens)
            .expect(&mut Sigil::Arrow)
            .unwrap());
    }

    #[test]
    fn keyword() {
        let mut params = [
            (Keyword::Let, "let"),
            (Keyword::When, "when"),
            (Keyword::Type, "type"),
            (Keyword::Match, "match"),
            (Keyword::Import, "import"),
        ];

        for (param, input) in params.iter_mut() {
            let filename = FileName::new("lex_keyword");

            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

            let test = Parser::new(filename, &input, &tokens)
                .parse_until_eof(param)
                .unwrap();

            assert_eq!(test.len(), 1);
            assert_eq!(test[0].value, *param);
        }
    }
}
