use super::*;

use diag::{FileName, Spanned};
use lexer::{Keyword, Sigil, Token};

impl<'a> Syntax<'a> for Token {
    type Output = Spanned<Self, FileName>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(self.clone())
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        Ok(parser.shift())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lex_token() {
        let mut syn = Token::Identifier;
        let input = "this is a test";
        let expected = input.split_whitespace().collect::<Vec<_>>();

        let input: diag::Text = input.into();
        let filename = FileName::new("lex_token");
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        let mut parser = crate::Parser::new(filename, &input, &tokens);

        let mut i = 0;
        while let Ok(d) = parser.expect(&mut syn) {
            if d.value == lexer::Token::EOF {
                break;
            }
            assert_eq!(parser.string_at(d.span).unwrap(), expected[i]);
            i += 1;
        }
    }

    #[test]
    fn lex_sigil() {
        let _ = env_logger::builder()
            .default_format_timestamp(false)
            .try_init();

        let mut syn = Token::Sigil(Sigil::Unit);

        let input: diag::Text = "(); ()".into();
        let filename = FileName::new("lex_sigil");
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        let mut parser = crate::Parser::new(filename, &input, &tokens);
        while let Ok(d) = parser.expect(&mut syn) {
            if d.value == lexer::Token::EOF {
                break;
            }
            eprintln!("{:#?}", d)
            // assert_eq!(Sigil::Unit, d.value);
        }
    }

    #[test]
    #[ignore]
    fn lex_keyword() {
        let mut syn = Token::Identifier;
        let input = "this is a test";
        let expected = input.split_whitespace().collect::<Vec<_>>();

        let input: diag::Text = input.into();
        let filename = FileName::new("lex_keyword");
        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .collect::<Vec<_>>();

        let mut parser = crate::Parser::new(filename, &input, &tokens);

        let mut i = 0;
        while let Ok(d) = parser.expect(&mut syn) {
            if d.value == lexer::Token::EOF {
                break;
            }
            assert_eq!(parser.string_at(d.span).unwrap(), expected[i]);
            i += 1;
        }
    }
}
