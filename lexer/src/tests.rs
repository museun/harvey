use super::*;
use diag::CurrentFile;

fn end(mut lex: impl Iterator<Item = Spanned<Token, CurrentFile>>) {
    assert_eq!(lex.next().unwrap().value, Token::EOF);
    assert!(lex.next().is_none());
}

#[test]
fn string() {
    // TODO test nested strings
    let inputs = &[
        ("\"testing\"", "testing"),
        ("\"testing testing\"", "testing testing"),
        ("\"testing 'testing'\"", "testing 'testing'"),
    ];

    for (input, expected) in inputs {
        let mut lexer = Lexer::new(&input, CurrentFile {}).into_iter();
        let next = lexer.next().unwrap();
        assert_eq!(next.value, Token::String);
        assert_eq!(&input[next.span], *expected);
        end(&mut lexer)
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
fn number() {
    let input = "1234567890";
    let expected = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];

    let lexer = Lexer::new(&input, CurrentFile {}).into_iter();
    let mut list: Vec<_> = lexer.collect();
    assert_eq!(list.pop().unwrap().value, Token::EOF);
    assert!(list.iter().all(|&k| k.value == Token::Integer));

    for (got, expected) in list.iter().zip(expected.iter()) {
        assert_eq!(&input[got.span], *expected)
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
    let next = list.pop().unwrap();;
    assert_eq!(next.value, Token::Integer);
    assert_eq!(&input[next.span], expected.pop().unwrap());

    assert_eq!(list.pop().unwrap().value, Token::Whitespace);
    assert_eq!(list.pop().unwrap().value, Token::Comment);
    assert_eq!(list.pop().unwrap().value, Token::Whitespace);

    let next = list.pop().unwrap();
    assert_eq!(next.value, Token::Integer);
    assert_eq!(&input[next.span], expected.pop().unwrap());
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
          = == != >= <= _ + : ; -> <- () }, ?! `++` >=<=",
        &[
            Token::Sigil(OpenBrace),
            Token::Sigil(CloseBrace),
            Token::Sigil(OpenParen),
            Token::Sigil(CloseParen),
            Token::Sigil(OpenBracket),
            Token::Sigil(CloseBracket),
            Token::Sigil(Question),
            Token::Sigil(Bang),
            Token::Sigil(Pipe),
            Token::Sigil(Greater),
            Token::Sigil(Less),
            Token::Sigil(Comma),
            Token::Sigil(Dot),
            Token::Sigil(Prime),
            Token::Sigil(Tick),
            Token::Sigil(Star),
            Token::Sigil(Backslash),
            Token::Sigil(Slash),
            Token::Sigil(Minus),
            Token::Sigil(Equal),
            Token::Sigil(EqualEqual),
            Token::Sigil(BangEqual),
            Token::Sigil(GreaterEqual),
            Token::Sigil(LessEqual),
            Token::Sigil(Underscore),
            Token::Sigil(Plus),
            Token::Sigil(Colon),
            Token::Sigil(SemiColon),
            Token::Sigil(Arrow),
            Token::Sigil(BackArrow),
            Token::Sigil(Unit),
            Token::Sigil(CloseBrace),
            Token::Sigil(Comma),
            Token::Sigil(Question),
            Token::Sigil(Bang),
            Token::Sigil(Tick),
            Token::Sigil(Plus),
            Token::Sigil(Plus),
            Token::Sigil(Tick),
            Token::Sigil(GreaterEqual),
            Token::Sigil(LessEqual),
            Token::EOF,
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
fn primitive() {
    let inputs = &[
        ("5_i8", Primitive::I8),
        ("5_i16", Primitive::I16),
        ("5_i32", Primitive::I32),
        ("5_i64", Primitive::I64),
        ("5_u8", Primitive::U8),
        ("5_u16", Primitive::U16),
        ("5_u32", Primitive::U32),
        ("5_u64", Primitive::U64),
        ("5_f32", Primitive::F32),
        ("5_f64", Primitive::F64),
    ];

    for (input, expected) in inputs {
        let mut lexer = Lexer::new(&input, CurrentFile {}).into_iter();
        assert_eq!(lexer.next().unwrap().value, Token::Integer);
        assert_eq!(lexer.next().unwrap().value, Token::Sigil(Sigil::Underscore));
        assert_eq!(lexer.next().unwrap().value, Token::Primitive(*expected));
        assert_eq!(lexer.next().unwrap().value, Token::EOF);
        assert!(lexer.next().is_none());
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
            .header(
                diag::Level::Warning,
                "where are all of the identifiers?",
                tok,
            )
            .line(tok.span.line(), &lines[tok.span.line() - 1])
            .flag(tok.span, "here's one!")
            // .note("something should go here")
            .build();
        eprintln!("{}", report);
    }
}
