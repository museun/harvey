use super::*;

pub struct Parser<'a> {
    filename: FileName,
    input: &'a Text,
    tokens: &'a [Spanned<lexer::Token, FileName>],

    previous: Span<FileName>,
    current: Span<FileName>,
    pos: usize,

    errors: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(
        filename: FileName,
        input: &'a Text,
        tokens: &'a [Spanned<lexer::Token, FileName>],
    ) -> Self {
        let (tok, pos) = eat(
            &tokens,
            &[lexer::Token::Whitespace, lexer::Token::Comment],
            0,
        );

        Self {
            input,
            tokens,
            filename,

            previous: tok.span,
            current: tok.span,
            pos,

            errors: vec![],
        }
    }

    pub fn is(&self, kind: impl PartialEq<lexer::Token>) -> bool {
        kind.eq(&self.tokens[self.pos].value)
    }

    pub fn current_span(&self) -> Span<FileName> {
        self.current
    }

    pub fn previous_span(&self) -> Span<FileName> {
        self.previous
    }

    pub fn peek(&self) -> Spanned<lexer::Token, FileName> {
        if self.pos + 1 >= self.tokens.len() {
            return self.tokens[self.tokens.len() - 1];
        }
        self.tokens[self.pos + 1]
    }

    pub fn string(&self, span: Span<FileName>) -> &'a str {
        &self.input[span]
    }

    pub fn shift(&mut self) -> Spanned<lexer::Token, FileName> {
        self.previous = self.current;
        let current = self.tokens[self.pos];
        self.pos += 1;
        current
    }

    pub fn discard_last_error(&mut self) -> Option<Diagnostic> {
        self.errors.pop()
    }

    pub fn parse_if_present<S>(
        &mut self,
        syntax: &mut S,
    ) -> Option<Result<S::Output, ErrorReported>>
    where
        S: Syntax<'a>,
    {
        log::trace!("parse? {:?} -> {:?}", syntax, self.peek());
        if self.test(syntax) {
            Some(self.expect(syntax))
        } else {
            None
        }
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
            self.eat(&[Whitespace, Comment]);
            if self.is(EOF) {
                break;
            }

            if self.test(&mut syntax) {
                match self.expect(&mut syntax) {
                    Ok(ok) => out.push(ok),
                    Err(ErrorReported(..)) => (),
                }
                self.eat(&[Whitespace, Comment]);
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
        log::trace!("test? {:?} -> {:?}", syntax, self.peek());
        syntax.test(self)
    }

    // TODO make this produce a Spanned<S::Output> with the aggregate of its parsers
    pub fn expect<S>(&mut self, syntax: &mut S) -> Result<S::Output, ErrorReported>
    where
        S: Syntax<'a>,
    {
        self.skip(lexer::Token::Whitespace);
        log::debug!("expect! {:?} -> {:?}", syntax, self.peek());
        syntax.expect(self)
    }

    pub fn report_error(&mut self, span: Span<FileName>, msg: impl ToString) -> ErrorReported {
        let diag = diag::Diagnostic::new(span, msg);

        #[cfg(feature = "trace")]
        {
            use diag::SpanFile;
            log::error!(
                "{}, got '{}' at {}:{}:{}",
                diag.message,
                &self.input[span],
                span.file().name(),
                span.line(),
                span.start()
            );
        }

        let err = ErrorReported::at_diagnostic(&diag);
        self.errors.push(diag);
        err
    }

    pub fn report_error_next(&mut self, msg: impl ToString) -> ErrorReported {
        self.report_error(self.peek().span, msg)
    }

    pub fn report_error_current(&mut self, msg: impl ToString) -> ErrorReported {
        self.report_error(self.current_span(), msg)
    }

    pub fn skip(&mut self, token: lexer::Token) -> bool {
        let mut count = 0;
        while self.is(token) {
            self.shift();
            count += 1;
        }
        count > 0
    }

    fn eat(&mut self, filtered: &[lexer::Token]) -> Spanned<lexer::Token, FileName> {
        let (span, pos) = eat(&self.tokens, filtered, self.pos);
        self.pos = pos;
        span
    }
}

fn eat(
    tokens: &[Spanned<lexer::Token, FileName>],
    filtered: &[lexer::Token],
    mut pos: usize,
) -> (Spanned<lexer::Token, FileName>, usize) {
    'outer: loop {
        let token = tokens[pos];
        if token.value == lexer::Token::EOF {
            break (token, pos);
        }
        for &filtered in filtered {
            if filtered == token.value {
                pos += 1;
                continue 'outer;
            }
        }
        break (token, pos);
    }
}
