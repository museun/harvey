use super::*;
use diag::{FileName, Spanned};

impl<'a> Syntax<'a> for Token {
    type Output = Spanned<Self, FileName>;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if self.test(parser) {
            let tok = parser.shift();
            Ok(diag::Spanned::new(tok.value, tok.span))
        } else {
            Err(parser.report_error(parser.peek().span, format!("expected '{}'", self)))
        }
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
                Ok(diag::Spanned::new(keyword, tok.span))
            } else {
                Err(parser.report_error(parser.peek().span, format!("wanted '{}'", self)))
            }
        } else {
            Err(parser.report_error(parser.peek().span, format!("expected '{}'", self)))
        }
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
                Ok(diag::Spanned::new(sigil, tok.span))
            } else {
                Err(parser.report_error(parser.peek().span, format!("wanted '{}'", self)))
            }
        } else {
            Err(parser.report_error(parser.peek().span, format!("expected '{}'", self)))
        }
    }
}

impl<'a> Syntax<'a> for Primitive {
    type Output = Spanned<Self, FileName>;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if self.test(parser) {
            let tok = parser.shift();
            if let Token::Primitive(prim) = tok.value {
                Ok(diag::Spanned::new(prim, tok.span))
            } else {
                Err(parser.report_error(parser.peek().span, format!("wanted '{}'", self)))
            }
        } else {
            Err(parser.report_error(parser.peek().span, format!("expected '{}'", self)))
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lex_token() {
        let filename = FileName::new("lex_token");

        let input = "this is a test";
        let expected = input.split_whitespace();

        let input: diag::Text = input.into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut parser = crate::Parser::new(filename, &input, &tokens);
        for (test, expected) in parser
            .parse_until_eof(&mut Token::Identifier)
            .unwrap()
            .iter()
            .zip(expected)
        {
            assert_eq!(parser.string_at(test.span).unwrap(), expected);
        }
    }

    #[test]
    fn lex_sigil() {
        let filename = FileName::new("lex_sigil");

        let input: diag::Text = "    () ()       ()\n   ()()()".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        assert!(crate::Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut Token::Sigil(Sigil::Unit))
            .unwrap()
            .iter()
            .all(|s| s.value == Token::Sigil(Sigil::Unit)));
    }

    #[test]
    fn lex_keyword() {
        let mut params = [
            (Keyword::Let, "let"),
            (Keyword::When, "when"),
            (Keyword::Type, "type"),
            (Keyword::Match, "match"),
            (Keyword::Import, "import"),
        ];

        for (param, input) in params.iter_mut() {
            let filename = FileName::new("lex_keyword");

            let input: diag::Text = input.clone().into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

            let test = crate::Parser::new(filename, &input, &tokens)
                .parse_until_eof(param)
                .unwrap();

            assert_eq!(test.len(), 1);
            assert_eq!(test[0].value, *param);
        }
    }
}
