use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

use diag::{Span, SpanFile, Spanned};

mod keyword;
mod literal;
mod sigil;
mod token;
mod unit;

pub use self::keyword::Keyword;
pub use self::literal::Literal;
pub use self::sigil::Sigil;
pub use self::token::{Invalid, Token};
pub use self::unit::UnitToken;

pub struct Lexer<'a, F: SpanFile> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    buf: VecDeque<Spanned<Token, F>>,

    // 1-based
    line: usize,
    // absolute
    index: usize,
    // byte offsets
    start: usize,
    pos: usize,

    current: Option<char>,
    file: F,
}

impl<'a, F: SpanFile> std::fmt::Debug for Lexer<'a, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer")
            .field("line", &self.line)
            .field("index", &self.index)
            .field("start", &self.start)
            .field("pos", &self.pos)
            .field("current", &self.current)
            .field("buf", &self.buf)
            .finish()
    }
}

impl<'a, F: SpanFile> Lexer<'a, F> {
    pub fn new(input: &'a str, file: F) -> Self {
        Self {
            input,
            iter: input.chars().peekable(),
            buf: VecDeque::new(),
            line: 1,
            index: 0,
            start: 0,
            pos: 0,
            current: None,
            file,
        }
    }

    fn lex(&mut self) -> Spanned<Token, F> {
        if let Some(tok) = self.buf.pop_front() {
            return tok;
        }

        let (current, next) = (self.advance(), self.peek().cloned());
        if current.is_none() {
            return self.emit(Token::EOF);
        }

        let tok = match (current.unwrap(), next) {
            ('\r', ..) => panic!("CRLF not supported yet"),
            (x @ '(', Some('*')) | (x @ '/', Some('/')) => self.comment(x),
            ('\n', ..) => self.emit(Token::NewLine),
            (c, ..) if c.is_ascii_whitespace() && c != '\n' => self.emit(Token::Whitespace),
            ('"', ..) => self.literal(),
            (c, ..) if c.is_ascii_digit() => self.literal(),
            (c, ..) if c.is_ascii_alphabetic() => self.ident(),
            (c, ..) if c.is_ascii_punctuation() => self.sigil(),
            (_, ..) => self.emit(Token::Invalid(Invalid::UnknownToken)),
        };

        if let Some(front) = self.buf.front() {
            use UnitToken as _;
            if std::mem::discriminant(&front.value) != std::mem::discriminant(&Sigil::unit()) {
                if let Some(front) = self.buf.pop_front() {
                    self.buf.push_back(tok);
                    return front;
                }
            }
        }

        log::debug!("tok: {:?}", tok);
        tok
    }

    fn comment(&mut self, ch: char) -> Spanned<Token, F> {
        self.advance(); // eat first one
        let single = ch == '/';

        let mut last = ch;
        let mut count = 0;

        loop {
            let d = match self.peek().cloned() {
                Some(d) => d,
                None if !single => return self.emit(Token::Invalid(Invalid::UnterminatedComment)),
                None => break,
            };
            match (last, d) {
                (.., '\n') if single => {
                    let tok = self.emit(Token::Comment);
                    self.buf.push_back(tok);
                    self.advance();
                    return self.emit(Token::NewLine);
                }
                ('*', ')') if count <= 1 => {
                    self.advance();
                    break;
                }
                ('*', ')') => count -= 1,
                ('(', '*') => count += 1,
                (.., d) => last = d,
            }
            self.advance();
        }
        self.emit(Token::Comment)
    }

    fn literal(&mut self) -> Spanned<Token, F> {
        let mut current;

        use Literal::*;
        macro_rules! start {
            ($lit:expr) => {{
                let tok = self.emit(Token::BeginLiteral($lit));
                current = $lit;
                self.buf.push_back(tok)
            }};
        }

        macro_rules! end {
            ($lit:expr) => {
                return self.emit(Token::EndLiteral($lit));
            };
        }

        match self.current.unwrap() {
            '"' => start!(String),
            '0' => match self.peek() {
                Some('x') => {
                    start!(Hexadecimal);
                    self.advance();
                }
                Some('o') => {
                    start!(Octal);
                    self.advance();
                }
                Some('b') => {
                    start!(Binary);
                    self.advance();
                }
                _ => start!(Integer),
            },
            _ => start!(Integer),
        }

        fn is_hex(c: char) -> bool {
            match c {
                '0'...'9' | 'a'...'f' | 'A'...'F' => true,
                _ => false,
            }
        }
        fn is_oct(c: char) -> bool {
            match c {
                '0'...'7' => true,
                _ => false,
            }
        }
        fn is_bin(c: char) -> bool {
            match c {
                '0' | '1' => true,
                _ => false,
            }
        }

        use fsm::State;
        #[derive(Clone, Copy, PartialEq, PartialOrd, fsm_derive::State)]
        enum DumbFloatShit {
            Characteristic,
            Fractional,
            Exponent,
            End,
        }
        use DumbFloatShit::*;
        let mut state = DumbFloatShit::start();

        // TODO determine where _ are invalid
        loop {
            match (current, self.peek()) {
                // this needs to be done later so the ends!() can happen
                (_, Some('_')) => {}

                (String, Some('"')) => {
                    // TODO count nested \
                    if self.current.unwrap() != '\\' {
                        self.advance();
                        end!(String)
                    }
                }

                (String, Some(..)) => (),

                (Integer, Some(c)) => match c {
                    '.' | 'E' | 'e' => {
                        self.buf.back_mut().unwrap().value = Token::BeginLiteral(Literal::Float);
                        current = Literal::Float;
                        state.goto(Fractional);
                    }
                    d if d.is_ascii_digit() => (),
                    _ => end!(Integer),
                },

                (Float, Some(c)) => match (state, c) {
                    (Characteristic, '.') => {
                        state.goto(Fractional);
                    }

                    (s, 'E') | (s, 'e') if s < Exponent => {
                        state.goto(Exponent);
                    }

                    (Exponent, '+') | (Exponent, '-') => {
                        state.goto(End);
                    }

                    // +/- can only follow e or E
                    (_, '+') | (_, '-') => end!(Float),

                    (_, d) if d.is_ascii_digit() => (),
                    _ => end!(Float),
                },

                (Hexadecimal, Some(&c)) if is_hex(c) => (),
                (Hexadecimal, Some(..)) => end!(Hexadecimal),

                (Octal, Some(&c)) if is_oct(c) => (),
                (Octal, Some(..)) => end!(Octal),

                (Binary, Some(&c)) if is_bin(c) => (),
                (Binary, Some(..)) => end!(Binary),

                (l, None) => end!(l),
            }
            self.advance();
        }
    }

    // // TODO handle nested strings
    // fn string(&mut self) -> Spanned<Token, F> {
    //     self.start += 1;
    //     while let Some(&ch) = self.peek() {
    //         let prev = self.current.unwrap();
    //         self.advance();
    //         if ch == '"' {
    //             if prev == '\\' {
    //                 continue;
    //             }
    //             self.pos -= 1;
    //             break;
    //         }
    //     }
    //     self.emit(Token::String)
    // }

    fn ident(&mut self) -> Spanned<Token, F> {
        let start = self.index - 1;
        let mut count = 1;

        while let Some(ch) = self.peek() {
            if ch.is_ascii_whitespace() || ch.is_ascii_punctuation() || ch.is_ascii_digit() {
                break;
            }
            self.advance();
            count += 1;
        }

        let s = &self.input[start..start + count];
        let tok = Keyword::lookup(&s)
            .map(Token::Keyword)
            .unwrap_or_else(|| Token::Identifier);
        self.emit(tok)
    }

    fn sigil(&mut self) -> Spanned<Token, F> {
        let start = self.index - 1;
        let mut pos = self.pos.saturating_sub(1);

        let mut count = 1;
        while let Some(ch) = self.peek() {
            if !ch.is_ascii_punctuation() || ch.is_ascii_whitespace() {
                break;
            }
            self.advance();
            count += 1;
        }

        let input = &self.input[start..start + count];
        assert!(self.buf.is_empty());

        let mut s = &input[..];
        let mut n = 0;
        let mut acc = 0;

        loop {
            if s.is_empty() || n == input.len() {
                break;
            }
            n += 1;
            if let Some(sigil) = Sigil::lookup(&s).map(Token::Sigil) {
                acc += s.len();
                pos += s.len();
                self.pos = pos;
                if std::mem::replace(&mut n, 0) == 0 {
                    return self.emit(sigil);
                }
                let tok = self.emit(sigil);
                self.buf.push_back(tok);
            }
            s = &input[acc..input.len() - n];
        }

        if self.buf.is_empty() {
            self.emit(Token::Invalid(Invalid::UnknownToken))
        } else {
            self.buf.pop_front().unwrap()
        }
    }

    fn emit(&mut self, kind: Token) -> Spanned<Token, F> {
        let start = self.start;
        let column = self.pos;

        // wrap around on newline
        let line = if kind == Token::NewLine {
            self.start = 0;
            self.pos = 0;
            self.line - 1
        } else {
            self.line
        };

        let span = Span::new(self.file, start, column, line);
        self.start = self.pos;
        Spanned::new(kind, span)
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn advance(&mut self) -> Option<char> {
        self.iter.next().map(|ch| {
            self.index += 1;
            self.pos += 1;
            if ch == '\n' {
                self.line += 1;
            }
            self.current.replace(ch);
            ch
        })
    }
}

impl<'a, F: SpanFile> IntoIterator for Lexer<'a, F> {
    type Item = Spanned<Token, F>;
    type IntoIter = LexerIter<'a, F>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIter {
            lexer: self,
            done: false,
        }
    }
}

pub struct LexerIter<'a, F: SpanFile> {
    lexer: Lexer<'a, F>,
    done: bool,
}

impl<'a, F: SpanFile> Iterator for LexerIter<'a, F> {
    type Item = Spanned<Token, F>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let tok = self.lexer.lex();
        if let Token::EOF = tok.value {
            self.done = true
        }
        Some(tok)
    }
}

impl<'a, F: SpanFile> std::iter::FusedIterator for LexerIter<'a, F> {}

#[cfg(test)]
mod tests;
