#![allow(dead_code, unused_imports, unused_variables)]
use fxhash::FxHasher;
use indexmap::{IndexMap, IndexSet};
use std::hash::BuildHasherDefault;

use diag::{Diagnostic, ErrorReported, FileName, Span, Spanned, Text};

pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub(crate) type FxIndexSet<K> = IndexSet<K, BuildHasherDefault<FxHasher>>;

mod syntax;
pub(crate) use syntax::Syntax;

mod parsers;
mod syntaxes;

pub struct Parser<'a> {
    input: &'a Text,
    tokens: &'a [Spanned<lexer::Token, FileName>],
    filename: FileName,

    last_span: Span<FileName>,
    lookahead_token: Spanned<lexer::Token, FileName>,
    next_lookahead: usize,

    errors: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(
        filename: FileName,
        input: &'a Text,
        tokens: &'a [Spanned<lexer::Token, FileName>],
    ) -> Self {
        let mut loc = 0;
        let tok = eat(
            filename,
            &input,
            &tokens,
            &[
                lexer::Token::Whitespace, //
                lexer::Token::Comment,
            ],
            &mut loc,
        );

        Self {
            input,
            tokens,
            filename,
            errors: vec![],
            last_span: Span::initial(filename),
            lookahead_token: tok,
            next_lookahead: loc,
        }
    }

    pub fn is(&self, kind: impl PartialEq<lexer::Token>) -> bool {
        kind.eq(&self.peek().value)
    }

    pub fn current_span(&self) -> Span<FileName> {
        self.last_span
    }

    pub fn peek(&self) -> Spanned<lexer::Token, FileName> {
        self.lookahead_token
    }

    pub fn peek_str(&self) -> &'a str {
        &self.input[self.peek().span]
    }

    pub fn string_at(&self, span: Span<FileName>) -> Option<&'a str> {
        self.input.get(span)
    }

    pub fn shift(&mut self) -> Spanned<lexer::Token, FileName> {
        self.last_span = self.lookahead_token.span;
        let last = self.lookahead_token;
        self.lookahead_token = self.eat(&[lexer::Token::Whitespace, lexer::Token::Comment]);
        log::trace!("shift: {:?} <- {:?}", last, self.lookahead_token);
        last
    }

    pub fn parse_until_eof<S>(
        &mut self,
        mut syntax: &mut S,
    ) -> Result<Vec<S::Output>, ErrorReported>
    where
        S: Syntax<'a>,
    {
        use lexer::Token::*;
        let mut out = vec![];
        loop {
            self.skip(NewLine);
            if self.is(EOF) {
                break;
            }
            if self.test(&mut syntax) {
                match self.expect(&mut syntax) {
                    Ok(ok) => out.push(ok),
                    Err(ErrorReported(..)) => (),
                }
            } else {
                let Spanned { span, .. } = self.shift();
                return Err(self.report_error(span, "unexpected token"));
            }
        }
        Ok(out)
    }

    pub fn test<S>(&self, syntax: &mut S) -> bool
    where
        S: Syntax<'a>,
    {
        log::trace!("test? {:?} -> {:?}", syntax, self.peek_str());
        syntax.test(self)
    }

    // TODO make this produce a Spanned<S::Output> with the aggregate of its parsers
    pub fn expect<S>(&mut self, syntax: &mut S) -> Result<S::Output, ErrorReported>
    where
        S: Syntax<'a>,
    {
        log::trace!("expect! {:?} -> {:?}", syntax, self.peek_str());
        syntax.expect(self)
    }

    pub fn report_error(&mut self, span: Span<FileName>, msg: impl ToString) -> ErrorReported {
        use diag::SpanFile;
        let diag = diag::Diagnostic::new(span, msg);
        if cfg!(test) {
            log::error!(
                "{}, got '{}' at {}:{}:{}",
                diag.message,
                &self.input[diag.span],
                span.file().name(),
                span.line(),
                span.start()
            );
        }
        let err = ErrorReported::at_diagnostic(&diag);
        self.errors.push(diag);
        err
    }

    fn skip(&mut self, token: lexer::Token) -> bool {
        let mut count = 0;
        while self.is(token) {
            self.shift();
            count += 1;
        }
        count > 0
    }

    fn eat(&mut self, filtered: &[lexer::Token]) -> Spanned<lexer::Token, FileName> {
        eat(
            self.filename,
            &self.input,
            &self.tokens,
            filtered,
            &mut self.next_lookahead,
        )
    }
}

fn eat<'a>(
    filename: FileName,
    source: &'a Text,
    tokens: &'a [Spanned<lexer::Token, FileName>],
    filtered: &[lexer::Token],
    pos: &mut usize,
) -> Spanned<lexer::Token, FileName> {
    'outer: loop {
        let token = tokens[*pos];
        if token.value == lexer::Token::EOF {
            return token;
        }
        *pos += 1;
        for &filtered in filtered {
            if filtered == token.value {
                continue 'outer;
            }
        }
        break token;
    }
}
