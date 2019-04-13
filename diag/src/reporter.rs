use super::*;

use colored::Colorize;

// TODO short format
// file:line:col: level: msg

#[derive(Debug, Default)]
pub struct Reporter {
    header: Option<String>,
    link: Option<String>,
    line: Option<String>,
    flag: Option<String>,
    note: Option<String>,
    level: Option<Level>,
    width: usize,
}

impl Reporter {
    pub fn header<T, F: SpanFile>(
        mut self,
        level: Level,
        msg: &str,
        spanned: Spanned<T, F>,
    ) -> Self {
        if self.header.is_some() || self.link.is_some() {
            return self;
        }

        let head = match level {
            Level::Info => "information".green().bold(),
            Level::Warning => "warning".yellow().bold(),
            Level::Error => "error".red().bold(),
        }
        .to_string();

        let mut header = String::new();
        header.push_str(&head);
        header.push_str(&": ".white().bold().to_string());
        header.push_str(&msg.white().bold().to_string());

        let mut link = String::new();
        link.push_str(&"-->".blue().to_string());
        link.push_str(&format!(
            " {}:{}:{}",
            spanned.span.file().name(),
            spanned.span.line(),
            spanned.range().start + 1
        ));

        self.header.replace(header);
        self.link.replace(link);
        self.level.replace(level);
        self
    }

    pub fn line(mut self, line_number: usize, msg: &str) -> Self {
        if self.line.is_some() {
            return self;
        }

        self.width = std::cmp::max(self.width, Self::digits(line_number));
        let mut line = String::new();
        line.push_str(&format!("{} |", line_number).blue().to_string());
        line.push_str("     ");
        line.push_str(msg);
        self.line.replace(line);
        self
    }

    pub fn flag<F: SpanFile>(mut self, span: Span<F>, description: &str) -> Self {
        if self.flag.is_some() {
            return self;
        }

        let level = self.level.expect("header must be called first");
        let mut buf = String::new();
        let marker = "^".repeat(span.end() - span.start());
        let (description, marker) = match level {
            Level::Info => (description.green().bold(), marker.green().bold()),
            Level::Warning => (description.yellow().bold(), marker.yellow().bold()),
            Level::Error => (description.red().bold(), marker.red().bold()),
        };

        buf.push_str(&" ".repeat(span.start()));
        buf.push_str(&marker.to_string());
        buf.push(' ');
        buf.push_str(&description.to_string());
        self.flag.replace(buf);
        self
    }

    pub fn note(mut self, msg: &str) -> Self {
        if self.note.is_some() {
            return self;
        }

        let mut note = String::new();
        note.push_str(&" = ".blue().to_string());
        note.push_str(&"note: ".white().bold().to_string());
        note.push_str(msg);
        self.note.replace(note);

        self
    }

    // TODO make this less panick-y
    pub fn build(mut self) -> String {
        struct Buffer(String, String, String);
        impl Buffer {
            fn new(hint: usize, pad: usize) -> Self {
                Self(
                    String::with_capacity(hint),
                    " |".blue().to_string(),
                    " ".repeat(pad),
                )
            }
            fn push(mut self, s: impl AsRef<str>) -> Self {
                self.0.push_str(s.as_ref());
                self
            }
            fn consume(self) -> String {
                self.0
            }
            fn pad(mut self) -> Self {
                self.0.push('\n');
                self.0.push_str(&self.2);
                self
            }
            fn guard(mut self) -> Self {
                self.0.push_str(&self.1);
                self
            }
            fn nl(mut self) -> Self {
                self.0.push('\n');
                self
            }
        }

        let header = self.header.take().expect("header must be set");
        let link = self.link.take().expect("link must be set");
        let line = self.line.take().expect("line must be set");
        let flag = self.flag.take().expect("flag must be set"); // only 1 flag for now

        let hint = header.len()
            + link.len()
            + line.len()
            + flag.len()
            + self.note.as_ref().map(String::len).unwrap_or_default()
            + 24;

        #[rustfmt::skip]
        let buf = Buffer::new(hint, self.width)
            // the header
            .push(header).pad()
            // the link line
            .push(link).pad().guard().nl()
            // source code line
            .push(line).pad()
            // the flag
            .guard().push("     ").push(flag).pad();

        let buf = if let Some(note) = self.note.take() {
            buf.push(note).nl()
        } else {
            buf
        };

        let mut string = buf.consume();
        string.shrink_to_fit();
        string
    }

    fn digits(d: usize) -> usize {
        if d == 0 {
            return 1;
        }

        let (mut x, mut d) = (0, d);
        while d > 0 {
            d /= 10;
            x += 1;
        }
        x
    }
}

// error[E0596]: cannot borrow `t` as mutable, as it is not declared as mutable
//    --> src/diagnostic.rs:184:9
//     |
// 183 |         let t = t.nl().consume();
//     |             - help: consider changing this to be mutable: `mut t`
// 184 |         t.shrink_to_fit();
//     |         ^ cannot borrow as mutable

// warning: variable does not need to be mutable
//    --> src/diagnostic.rs:135:20
//     |
// 135 |             fn pad(mut self) -> Self {
//     |                    ----^^^^
//     |                    |
//     |                    help: remove this `mut`

// error[E0382]: borrow of moved value: `self`
//    --> src/diagnostic.rs:137:25
//     |
// 136 |                 let this = self.nl();
//     |                            ---- value moved here
// 137 |                 let s = &self.2;
//     |                         ^^^^^^^ value borrowed here after move
//     |
//     = note: move occurs because `self` has type `diagnostic::Printer::build::Builder`, which does not
// implement the `Copy` trait

// error[E0502]: cannot borrow `*parser` as mutable because it is also borrowed as immutable
//    --> src/lib.rs:430:21
//     |
// 422 |         let data = parser.peek_str();
//     |                    ------ immutable borrow occurs here
// ...
// 430 |         let value = data.intern(parser);
//     |                     ^^^^^------^^^^^^^^
//     |                     |    |
//     |                     |    immutable borrow later used by call
//     |                     mutable borrow occurs here

// error: aborting due to 3 previous errors

// error[E0515]: cannot return value referencing local variable `tokens`
//   --> parser\src\syntax.rs:99:9
//    |
// 99 |         (crate::Parser::new(file, &input, &tokens, 0), tokens)
//    |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-------^^^^^^^^^^^^^
//    |         |                                 |
//    |         |                                 `tokens` is borrowed here
//    |         returns a value referencing data owned by the current function

// error[E0515]: cannot return value referencing local variable `input`
//   --> parser\src\syntax.rs:99:9
//    |
// 99 |         (crate::Parser::new(file, &input, &tokens, 0), tokens)
//    |         ^^^^^^^^^^^^^^^^^^^^^^^^^^------^^^^^^^^^^^^^^^^^^^^^^
//    |         |                         |
//    |         |                         `input` is borrowed here
//    |         returns a value referencing data owned by the current function

// error[E0505]: cannot move out of `tokens` because it is borrowed
//   --> parser\src\syntax.rs:99:56
//    |
// 85 |     pub fn new_parser<'a>(
//    |                       -- lifetime `'a` defined here
// ...
// 99 |         (crate::Parser::new(file, &input, &tokens, 0), tokens)
//    |         -----------------------------------------------^^^^^^-
//    |         |                                 |            |
//    |         |                                 |            move out of `tokens` occurs here
//    |         |                                 borrow of `tokens` occurs here
//    |         returning this value requires that `tokens` is borrowed for `'a`

// error: aborting due to 3 previous errors
