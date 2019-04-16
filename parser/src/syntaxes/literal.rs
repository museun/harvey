use super::*;

#[derive(Debug)]
pub struct Literal;
impl<'a> Syntax<'a> for Literal {
    type Output = hir::Literal;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut LitNumber) || parser.test(&mut LitFloat) || parser.test(&mut LitString)
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        match (
            parser.test(&mut LitNumber),
            parser.test(&mut LitFloat),
            parser.test(&mut LitString),
        ) {
            (true, _, _) => LitNumber.expect(parser),
            (_, true, _) => LitFloat.expect(parser),
            (_, _, true) => LitString.expect(parser),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct LitString;
impl<'a> Syntax<'a> for LitString {
    type Output = hir::Literal;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(lexer::Literal::String)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let diag::Spanned { span, .. } = parser.expect(&mut lexer::Literal::String)?;
        let s = parser.string(span);
        let s = if s.len() >= 2 { &s[1..s.len() - 1] } else { s };
        Ok(hir::Literal::String(s.to_string()))
    }
}

#[derive(Debug)]
pub struct LitNumber;
impl<'a> Syntax<'a> for LitNumber {
    type Output = hir::Literal;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::Minus)
            || parser.test(&mut lexer::Literal::Integer)
            || parser.test(&mut lexer::Literal::Hexadecimal)
            || parser.test(&mut lexer::Literal::Octal)
            || parser.test(&mut lexer::Literal::Binary)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let neg = if parser.test(&mut Sigil::Minus) {
            parser.shift();
            true
        } else {
            false
        };
        use lexer::Literal::*;

        let (mut kind, radix) = match (
            parser.test(&mut Integer),
            parser.test(&mut Hexadecimal),
            parser.test(&mut Octal),
            parser.test(&mut Binary),
        ) {
            (true, _, _, _) => (Integer, 10),
            (_, true, _, _) => (Hexadecimal, 16),
            (_, _, true, _) => (Octal, 8),
            (_, _, _, true) => (Binary, 2),
            s => unreachable!("{:?}", s),
        };

        let diag::Spanned { span, .. } = parser.expect(&mut kind)?;
        parse_string(&parser.string(span), radix)
            .map(|k| if neg { -k } else { k })
            .map(hir::Literal::Integer)
            .map_err(|d| parser.report_error(span, format!("invalid {} literal at {}", kind, d)))
    }
}

#[derive(Debug)]
pub struct LitFloat;
impl<'a> Syntax<'a> for LitFloat {
    type Output = hir::Literal;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::Minus)
            || parser.test(&mut lexer::Literal::Float)
            || parser.test(&mut Token::Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let neg = if parser.test(&mut Sigil::Minus) {
            parser.shift();
            true
        } else {
            false
        };

        if parser.test(&mut Token::Identifier) {
            let diag::Spanned { span, .. } = parser.expect(&mut Token::Identifier)?;
            match parser.current_str() {
                s @ "inf" | s @ "NaN" => {
                    return [if neg { "-" } else { "" }, s]
                        .concat()
                        .parse()
                        .map(hir::Literal::Float)
                        .map_err(|_| parser.report_error(span, "invalid float literal"))
                }
                e => unreachable!("unreachable float string {:?}", e),
            }
        }

        let diag::Spanned { span, .. } = parser.expect(&mut lexer::Literal::Float)?;

        let mut periods = false;
        let mut signs = false;
        let mut exponents = false;

        // TODO subspan
        macro_rules! float_err {
            ($msg:expr) => {{
                return Err(parser.report_error(span, $msg));
            }};
            ($f:expr, $($args:expr),* $(,)?)  => {{
                return Err(parser.report_error(span, format!($f, $($args,)*)));
            }};
        }

        fn valid(last: Option<char>, ch: char) -> bool {
            if last != Some('.') || last.is_none() {
                return true;
            }

            match ch {
                'e' | 'E' | '+' | '-' | '.' | '_' => false,
                _ => true,
            }
        }

        let string = parser.string(span);
        let mut last = None;
        let mut buf = String::with_capacity(string.len());
        for ch in string.chars() {
            match ch {
                d if !valid(last, d) => float_err!("{} cannot follow .", d),
                d if d.is_ascii_digit() => (),

                'E' | 'e' if exponents => float_err!("too many exponents"),
                'E' | 'e' => exponents = true,
                '-' | '+' if signs => float_err!("too many signs"),
                '-' | '+' => signs = true,
                '.' if periods => float_err!("too many periods"),
                '.' => periods = true,

                '_' => continue,

                d => float_err!("invalid character: {}", d),
            }
            buf.push(ch);
            last.replace(ch);
        }

        if neg {
            buf.insert(0, '-')
        }

        buf.parse()
            .map(hir::Literal::Float)
            .map_err(|_| parser.report_error(span, "invalid float literal"))
    }
}

fn parse_string(input: &str, radix: i64) -> std::result::Result<i64, usize> {
    let head = match radix {
        16 | 8 | 2 => 2,
        _ => 0,
    };
    if input.len() <= head {
        return Err(input.len());
    }

    let mut out = 0;
    for (i, ch) in input.chars().enumerate().skip(head) {
        if ch == '_' {
            if i <= head {
                return Err(i);
            }
            continue;
        }
        let ch = ch.to_digit(radix as u32).map(i64::from).ok_or_else(|| i)?;
        out = out * radix + ch;
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literals() {
        let expected = vec![
            hir::Literal::Integer(0x123),
            hir::Literal::Float(45.67),
            hir::Literal::String("foobar".into()),
        ];
        let input: diag::Text = "0x1_2_3 45.67 \"foobar\"".into();

        let filename = diag::FileName::new("literal");
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

        let mut syntax =
            crate::parsers::Then::new(Literal, crate::parsers::Sink::new(Token::Whitespace));

        Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut syntax)
            .unwrap()
            .iter()
            .zip(expected.iter())
            .for_each(|(test, expected)| assert_eq!(test, expected));
    }

    #[test]
    fn multi_integer() {
        let filename = diag::FileName::new("multi");
        let input: diag::Text = "1234 -4321 -5678 8765".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut LitNumber)
                .unwrap(),
            vec![
                hir::Literal::Integer(1234),
                hir::Literal::Integer(-4321),
                hir::Literal::Integer(-5678),
                hir::Literal::Integer(8765),
            ]
        );
    }

    #[test]
    fn literal_string() {
        let filename = diag::FileName::new("literal_string");
        let input: diag::Text = "\"foobar\"".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .expect(&mut LitString)
                .unwrap(),
            hir::Literal::String("foobar".into())
        );

        let input: diag::Text = "\"\"".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .expect(&mut LitString)
                .unwrap(),
            hir::Literal::String("".into())
        );
    }

    #[test]
    fn literal_hexadecimal() {
        let filename = diag::FileName::new("literal_hexadecimal");
        let inputs = &[
            ("-0x1024", -4132),
            ("0x1024", 4132),
            ("-0x1_024", -4132),
            ("0x10___24", 4132),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            assert_eq!(
                Parser::new(filename, &input, &tokens)
                    .expect(&mut LitNumber)
                    .expect("valid hexadecimal"),
                hir::Literal::Integer(*expected)
            );
        }
    }

    #[test]
    fn literal_octal() {
        let filename = diag::FileName::new("literal_octal");
        let inputs = &[
            ("-0o033", -27),
            ("0o033", 27),
            ("-0o0_3_3", -27),
            ("0o03_3___", 27),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            assert_eq!(
                Parser::new(filename, &input, &tokens)
                    .expect(&mut LitNumber)
                    .expect("valid octal"),
                hir::Literal::Integer(*expected)
            );
        }
    }

    #[test]
    fn literal_binary() {
        let filename = diag::FileName::new("literal_binary");
        let inputs = &[
            ("-0b10110010", -178),
            ("0b10110010", 178),
            ("-0b1011_0010", -178),
            ("0b10_11_00_10", 178),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            assert_eq!(
                Parser::new(filename, &input, &tokens)
                    .expect(&mut LitNumber)
                    .expect("valid binary"),
                hir::Literal::Integer(*expected)
            );
        }
    }

    #[test]
    fn literal_integer() {
        let filename = diag::FileName::new("literal_integer");
        let inputs = &[
            ("1234", 1234),
            ("-1234", -1234),
            ("1_000_000", 1_000_000),
            ("-1_000_000", -1_000_000),
            ("1_2__3___", 123),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();

            assert_eq!(
                Parser::new(filename, &input, &tokens)
                    .expect(&mut LitNumber)
                    .expect("valid integer"),
                hir::Literal::Integer(*expected)
            );
        }

        let input: diag::Text = "1234 4321".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut LitNumber)
                .unwrap(),
            vec![hir::Literal::Integer(1234), hir::Literal::Integer(4321)]
        );
    }

    #[test]
    fn bad_number_literals() {
        let filename = diag::FileName::new("bad_number_literals");
        for bad in &["0x_1234", "0xGG", "0b_1", "0b77", "0o_1", "0o9"] {
            let bad: diag::Text = (*bad).into();
            let tokens = Lexer::new(&bad, filename).into_iter().collect::<Vec<_>>();

            Parser::new(filename, &bad, &tokens)
                .expect(&mut LitNumber)
                .unwrap_err();
        }
    }

    #[test]
    fn literal_float() {
        let filename = diag::FileName::new("literal_float");

        #[allow(clippy::approx_constant)]
        let inputs = &[
            ("3.14", 3.14),
            ("-3.14", -3.14),
            ("2.5E10", 2.5E10),
            ("2.5e10", 2.5e10),
            ("2.5E-10", 2.5E-10),
            ("0.5", 0.5),
            ("1.0e+1-1", 10.0),
            ("inf", std::f64::INFINITY),
            ("-inf", std::f64::NEG_INFINITY),
            ("NaN", std::f64::NAN),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            let t = Parser::new(filename, &input, &tokens)
                .expect(&mut LitFloat)
                .unwrap();
            if expected.is_nan() {
                assert!(if let hir::Literal::Float(f) = t {
                    f.is_nan() == expected.is_nan()
                } else {
                    false
                });
                continue;
            }
            assert_eq!(t, hir::Literal::Float(*expected));
        }
    }

    #[test]
    fn bad_float_literals() {
        let filename = diag::FileName::new("bad_float_literals");
        // TODO "1.+1",
        let bad = &["_1.1", "1._1", "._1", "1.e1", ".e1", ".+1", "1.1e/1"];
        for bad in bad {
            eprintln!("---> `{}`", bad);
            let input: diag::Text = (*bad).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            Parser::new(filename, &input, &tokens)
                .expect(&mut LitFloat)
                .unwrap_err();
        }
    }
}
