use super::*;

use diag::{FileName, Spanned};
use lexer::{Keyword, Sigil, Token};

impl<'a> Syntax<'a> for Token {
    type Output = Spanned<Self, FileName>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if self.test(parser) {
            Ok(parser.shift())
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
        let input = "this is a test";
        let expected = input.split_whitespace();

        let input: diag::Text = input.into();
        let filename = FileName::new("lex_token");
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        for (test, expected) in crate::Parser::new(filename, &input, &tokens)
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
        let input: diag::Text = "    () ()       ()\n   ()()()".into();
        let filename = FileName::new("lex_sigil");
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        assert!(crate::Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut Token::Sigil(Sigil::Unit))
            .unwrap()
            .iter()
            .all(|s| s.value == lexer::Token::Sigil(Sigil::Unit)));
    }

    #[test]
    fn lex_keyword() {
        let params = &[
            (Keyword::Let, "let"),
            (Keyword::When, "when"),
            (Keyword::Type, "type"),
            (Keyword::Match, "match"),
            (Keyword::Import, "import"),
        ];

        for (param, input) in params {
            let input: diag::Text = input.clone().into();
            let filename = FileName::new("lex_keyword");
            let tokens = lexer::Lexer::new(&input, filename)
                .into_iter()
                .collect::<Vec<_>>();

            let test = crate::Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut Token::Keyword(*param))
                .unwrap();
            assert_eq!(test.len(), 1);
            assert_eq!(test[0].value, Token::Keyword(*param));
        }
    }
}
