use super::*;
use diag::CurrentFile;

fn end(mut lex: impl Iterator<Item = Spanned<Token, CurrentFile>>) {
    assert_eq!(lex.next().unwrap().value, Token::EOF);
    assert!(lex.next().is_none());
}

fn test_begin_end(lit: Literal, input: &str, expected: &str) {
    let mut lexer = Lexer::new(&input, CurrentFile {}).into_iter();

    let fst = lexer.next().unwrap();
    assert_eq!(fst.value, Token::BeginLiteral(lit));

    let snd = lexer.next().unwrap();
    assert_eq!(snd.value, Token::EndLiteral(lit));

    assert_eq!(&input[fst.span.extend(snd.span)], expected);
    end(&mut lexer)
}

#[test]
fn string() {
    // TODO test nested strings
    let inputs = &[
        "\"testing\"",
        "\"testing testing\"",
        "\"testing 'testing'\"",
    ];

    for input in inputs {
        test_begin_end(Literal::String, input, input)
    }
}

#[test]
fn decimal() {
    for input in &["12345", "012345", "1_2_3_4_5", "1_____0", "1"] {
        test_begin_end(Literal::Integer, input, input)
    }
}

#[test]
fn hexadecimal() {
    let inputs = &["0x12345", "0xFF_FF_FF", "0xF"];
    for input in inputs {
        test_begin_end(Literal::Hexadecimal, input, input)
    }
}

#[test]
fn octal() {
    let inputs = &["0o0_00", "0o777"];
    for input in inputs {
        test_begin_end(Literal::Octal, input, input)
    }
}

#[test]
fn binary() {
    let inputs = &["0b0____0", "0b10101010101", "0b1", "0b0"];
    for input in inputs {
        test_begin_end(Literal::Binary, input, input)
    }
}

#[test]
fn float() {
    for input in &[
        "1_0_0_.0e1",
        "1_0_0.0_e1",
        "1_0_0.0e_1",
        "1_0_0.0e1",
        "1_0_0.0e1_",
        "1_0_0._0e1",
        "1e10",
        "100.",
        "100.0",
        "0.100",
    ] {
        test_begin_end(Literal::Float, input, input)
    }
}

#[test]
fn ident() {
    let inputs = &["asdf", "this", "sum"];

    for input in inputs {
        let mut lexer = Lexer::new(&input, CurrentFile {}).into_iter();
        let next = lexer.next().unwrap();
        assert_eq!(next.value, Token::Identifier);
        assert_eq!(&input[next.span], *input);
        end(&mut lexer)
    }
}

#[test]
fn comment() {
    let input = "this is a // test";
    let expected = ["this", "is", "a", "test"];

    let mut list = Lexer::new(input, CurrentFile {})
        .into_iter()
        .collect::<Vec<_>>();
    assert_eq!(list.pop().unwrap().value, Token::EOF);
    assert_eq!(list.pop().unwrap().value, Token::Comment);

    for (got, expected) in list
        .iter()
        .enumerate()
        .filter_map(|(i, v)| if i == 0 || i & 1 == 0 { Some(v) } else { None })
        .zip(expected.iter())
    {
        assert_eq!(&input[got.span], *expected)
    }

    let input = "1 (* and 2 *) 3";
    let mut expected = vec!["1", "3"];

    let mut list = Lexer::new(input, CurrentFile {})
        .into_iter()
        .collect::<Vec<_>>();
    assert_eq!(list.pop().unwrap().value, Token::EOF);

    let b = list.pop().unwrap();
    let a = list.pop().unwrap();
    assert_eq!(a.value, Token::BeginLiteral(Literal::Integer));
    assert_eq!(b.value, Token::EndLiteral(Literal::Integer));
    assert_eq!(&input[a.span.extend(b.span)], expected.pop().unwrap());

    assert_eq!(list.pop().unwrap().value, Token::Whitespace);
    assert_eq!(list.pop().unwrap().value, Token::Comment);
    assert_eq!(list.pop().unwrap().value, Token::Whitespace);

    let b = list.pop().unwrap();
    let a = list.pop().unwrap();
    assert_eq!(a.value, Token::BeginLiteral(Literal::Integer));
    assert_eq!(b.value, Token::EndLiteral(Literal::Integer));
    assert_eq!(&input[a.span.extend(b.span)], expected.pop().unwrap());
}

#[test]
fn sigils() {
    use Sigil::*;
    fn test(input: &str, expected: &[Token]) {
        use std::mem::discriminant as comp;
        let null = Token::Sigil(Sigil::Unit);

        for (i, (test, expected)) in Lexer::new(&input, CurrentFile {})
            .into_iter()
            .filter(|k| comp(&k.value) == comp(&null))
            .zip(expected.iter())
            .enumerate()
        {
            assert_eq!(test.value, *expected, "failure at {}", i)
        }
    }

    test(
        " { } ( ) [ ] ? ! | > < , . ' ` * \\ / -
          = == != >= <= _ + : ; -> <- () ,} }, ?! `++` >=<="
            .trim(),
        &[
            /* 00 */ Token::Sigil(OpenBrace),
            /* 01 */ Token::Sigil(CloseBrace),
            /* 02 */ Token::Sigil(OpenParen),
            /* 03 */ Token::Sigil(CloseParen),
            /* 04 */ Token::Sigil(OpenBracket),
            /* 05 */ Token::Sigil(CloseBracket),
            /* 06 */ Token::Sigil(Question),
            /* 07 */ Token::Sigil(Bang),
            /* 08 */ Token::Sigil(Pipe),
            /* 09 */ Token::Sigil(Greater),
            /* 10 */ Token::Sigil(Less),
            /* 11 */ Token::Sigil(Comma),
            /* 12 */ Token::Sigil(Dot),
            /* 13 */ Token::Sigil(Prime),
            /* 14 */ Token::Sigil(Tick),
            /* 15 */ Token::Sigil(Star),
            /* 16 */ Token::Sigil(Backslash),
            /* 17 */ Token::Sigil(Slash),
            /* 18 */ Token::Sigil(Minus),
            /* 19 */ Token::Sigil(Equal),
            /* 20 */ Token::Sigil(EqualEqual),
            /* 21 */ Token::Sigil(BangEqual),
            /* 22 */ Token::Sigil(GreaterEqual),
            /* 23 */ Token::Sigil(LessEqual),
            /* 24 */ Token::Sigil(Underscore),
            /* 25 */ Token::Sigil(Plus),
            /* 26 */ Token::Sigil(Colon),
            /* 27 */ Token::Sigil(SemiColon),
            /* 28 */ Token::Sigil(Arrow),
            /* 29 */ Token::Sigil(BackArrow),
            /* 30 */ Token::Sigil(Unit),
            /* 31 */ Token::Sigil(Comma),
            /* 32 */ Token::Sigil(CloseBrace),
            /* 33 */ Token::Sigil(CloseBrace),
            /* 34 */ Token::Sigil(Comma),
            /* 35 */ Token::Sigil(Question),
            /* 36 */ Token::Sigil(Bang),
            /* 37 */ Token::Sigil(Tick),
            /* 38 */ Token::Sigil(Plus),
            /* 39 */ Token::Sigil(Plus),
            /* 40 */ Token::Sigil(Tick),
            /* 41 */ Token::Sigil(GreaterEqual),
            /* 42 */ Token::Sigil(LessEqual),
            /* 43 */ Token::EOF,
        ],
    );

    test(
        ";();==;<=;>=;",
        &[
            Token::Sigil(SemiColon),
            Token::Sigil(Unit),
            Token::Sigil(SemiColon),
            Token::Sigil(EqualEqual),
            Token::Sigil(SemiColon),
            Token::Sigil(LessEqual),
            Token::Sigil(SemiColon),
            Token::Sigil(GreaterEqual),
            Token::Sigil(SemiColon),
        ],
    );

    test(
        r#"match foo {
            a when a == 2 -> { () },
            _         -> { a <- b }
        }"#,
        &[
            Token::Sigil(OpenBrace),
            Token::Sigil(EqualEqual),
            Token::Sigil(Arrow),
            Token::Sigil(OpenBrace),
            Token::Sigil(Unit),
            Token::Sigil(CloseBrace),
            Token::Sigil(Comma),
            Token::Sigil(Underscore),
            Token::Sigil(Arrow),
            Token::Sigil(OpenBrace),
            Token::Sigil(BackArrow),
            Token::Sigil(CloseBrace),
            Token::Sigil(CloseBrace),
        ],
    );
}

#[test]
fn keywords() {
    let input =
            "if type then that enum while okay impl match when let this return or break for the continue";

    let expected = [
        Token::Identifier,
        Token::Keyword(Keyword::Type),
        Token::Identifier,
        Token::Identifier,
        Token::Keyword(Keyword::Enum),
        Token::Identifier,
        Token::Identifier,
        Token::Keyword(Keyword::Impl),
        Token::Keyword(Keyword::Match),
        Token::Keyword(Keyword::When),
        Token::Keyword(Keyword::Let),
        Token::Identifier,
        Token::Keyword(Keyword::Return),
        Token::Identifier,
        Token::Keyword(Keyword::Break),
        Token::Keyword(Keyword::For),
        Token::Identifier,
        Token::Keyword(Keyword::Continue),
    ];
    let mut list = Lexer::new(&input, CurrentFile {})
        .into_iter()
        .collect::<Vec<_>>();

    assert_eq!(list.pop().unwrap().value, Token::EOF);

    for (k, v) in list
        .iter()
        .filter(|&k| k.value != Token::Whitespace && k.value != Token::NewLine)
        .map(|k| k.value)
        .zip(expected.iter())
    {
        assert_eq!(k, *v);
    }
}

#[test]
#[ignore]
fn demo() {
    let input: diag::Text = include_str!("../../foobar.harvey").into();
    let file = diag::FileName::new("./foobar.harvey");

    let lines = input.split('\n').collect::<Vec<_>>();

    for tok in Lexer::new(&input, file)
        .into_iter()
        .filter(|&k| k.value == Token::Identifier)
    {
        let report = diag::Reporter::default()
            .header(diag::Level::Info, "where are all of the identifiers?", tok)
            .line(tok.span.line(), &lines[tok.span.line() - 1])
            .flag(tok.span, "here's one!")
            // .note("something should go here")
            .build();
        eprintln!("{}", report);
    }
}
