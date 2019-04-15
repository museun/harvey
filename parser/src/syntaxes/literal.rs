use super::*;

#[derive(Debug)]
pub struct Literal;
impl<'a> Syntax<'a> for Literal {
    type Output = hir::Literal;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut LitInteger) || parser.test(&mut LitFloat) || parser.test(&mut LitString)
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        match (
            parser.test(&mut LitInteger) || parser.test(&mut LitFloat),
            parser.test(&mut LitString),
        ) {
            (true, false) => {
                let mut checkpoint = parser.checkpoint();
                LitFloat
                    .expect(&mut checkpoint)
                    .and_then(|ok| {
                        checkpoint.commit();
                        Ok(ok)
                    })
                    .or_else(|_| LitInteger.expect(parser))
            }
            (false, true) => LitString.expect(parser),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct LitString;
impl<'a> Syntax<'a> for LitString {
    type Output = hir::Literal;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::String)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let diag::Spanned { span, .. } = parser.expect(&mut Token::String)?;
        Ok(hir::Literal::String(parser.string(span).to_string()))
    }
}

#[derive(Debug)]
pub struct LitInteger;
impl<'a> Syntax<'a> for LitInteger {
    type Output = hir::Literal;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::Minus) || parser.is(Token::Integer)
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let neg = parser.is(Sigil::Minus);
        if neg {
            parser.shift();
        }
        enum Mode {
            Dec(char),
            Hex,
            Oct,
            Bin,
        }
        use Mode::*;

        let diag::Spanned { span, .. } = parser.expect(&mut Token::Integer)?;
        let mode = match dbg!(parser.string(span)).chars().next().unwrap() {
            '0' if parser.is(Token::Identifier) => {
                let diag::Spanned { span, .. } = parser.expect(&mut Token::Identifier)?;
                match parser.string(span).chars().next() {
                    Some('x') => Mode::Hex,
                    Some('o') => Mode::Oct,
                    Some('b') => Mode::Bin,
                    Some(d) => {
                        let msg = format!("invalid suffix: {}", d);
                        return Err(parser.report_error_current(msg));
                    }
                    _ => unimplemented!(),
                }
            }
            c => Mode::Dec(c),
        };

        let mut out = 0i64;
        macro_rules! parse {
            (base $base:expr, $syntax:expr, msg=> $msg:expr) => {{
                if !parser.test(&mut $syntax) {
                    return Err(parser.report_error_next($msg));
                }
                loop {
                    if !parser.test(&mut $syntax) {
                        break;
                    }
                    parser.expect(&mut $syntax)?;
                    for d in parser.current_str().chars().map(|d| {
                        d.to_digit($base)
                            .map(i64::from)
                            .ok_or_else(|| parser.report_error(parser.current_span(), $msg))
                    }) {
                        out = $base * out + d?
                    }
                    if parser.is(Token::Whitespace) || parser.is(Token::EOF) {
                        break;
                    }
                    if !parser.is(Token::Integer) && !parser.is(Sigil::Underscore) {
                        return Err(parser.report_error_next("invalid integer"));
                    }
                    parser.expect(&mut Sink::new(Sigil::Underscore))?;
                }
                out
            }};
        }

        let out = match mode {
            Dec(head) => {
                parser.expect(&mut Sink::new(Sigil::Underscore))?;
                out = head.to_digit(10).map(i64::from).unwrap();
                parse!(base 10, Token::Integer, msg=> "invalid decimal digit")
            }
            Hex => {
                let mut syntax = Or::new(Token::Identifier, Token::Integer);
                parse!(base 16, syntax, msg=> "invalid hexadecimal digit")
            }
            Oct => parse!(base 8,  Token::Integer, msg=> "invalid octal digit"),
            Bin => parse!(base 2,  Token::Integer, msg=> "invalid binary digit"),
        };

        parser.expect(&mut Token::Integer)?;
        dbg!(Ok(hir::Literal::Integer(if neg { -out } else { out })))
    }
}

#[derive(Debug)]
pub struct LitFloat;
impl<'a> Syntax<'a> for LitFloat {
    type Output = hir::Literal;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::Minus) || parser.is(Token::Integer)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let neg = parser.test(&mut Sigil::Minus);
        if neg {
            parser.shift();
        }

        macro_rules! err {
            ($msg:expr) => {
                return Err(parser.report_error_current($msg));
            };
        }

        if parser.test(&mut Token::Identifier) {
            let diag::Spanned { span, .. } = parser.expect(&mut Token::Identifier)?;
            let s = parser.string(span);
            return match s {
                "NaN" => Ok(hir::Literal::Float(std::f64::NAN)),
                "inf" => Ok(hir::Literal::Float(if neg {
                    std::f64::NEG_INFINITY
                } else {
                    std::f64::INFINITY
                })),
                d => err!(format!("unknown float literal: {}", d)),
            };
        }

        use fsm::State as _;
        #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, fsm_derive::State)]
        enum Float {
            Integral,
            Fractional,
            Exponent,
            End,
        }

        let mut start = dbg!(parser.current_span());
        let mut float = Float::start();
        let mut previous = dbg!(parser.current_str());
        loop {
            let current = parser.current_str().trim(); // HMM
            match current {
                "E" | "e" => {
                    if previous == "." {
                        err!("cannot place . here")
                    }
                    float.goto(Float::Exponent);
                }
                "+" | "-" => {
                    float.goto(Float::End);
                }
                "." => {
                    float.goto(Float::Fractional);
                }
                "_" => {}
                d => {
                    if !d
                        .chars()
                        .inspect(|k| eprintln!("{:?}", k))
                        .all(|d| d.is_ascii_digit())
                    {
                        eprintln!("not all digits");
                        break;
                    }
                }
            }

            if (parser.is(Sigil::Minus) || parser.is(Sigil::Plus)) && float == Float::End {
                break;
            }

            let diag::Spanned { span, value, .. } = parser.shift();
            if value == Token::EOF {
                break;
            }
            start = start.extend(span);
            previous = current;
        }

        dbg!(Ok(hir::Literal::Float(
            parser
                .string(start)
                .parse()
                .or_else(|_| Err(parser.report_error(start, "invalid float literal")),)?
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literals() {
        // let _ = env_logger::builder()
        //     .default_format_timestamp(false)
        //     .try_init();

        let expected = vec![
            hir::Literal::Integer(0x123),
            hir::Literal::Float(45.67),
            hir::Literal::String("foobar".into()),
        ];
        let input: diag::Text = "0x1_2_3 45.67 \"foobar\"".into();

        let filename = diag::FileName::new("literal");
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut Literal)
            .unwrap()
            .iter()
            .zip(expected.iter())
            .for_each(|(test, expected)| assert_eq!(test, expected));
    }

    #[test]
    fn multi_integer() {
        let filename = diag::FileName::new("multi");
        let input: diag::Text = "1234 4321 5678 8765".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut LitInteger)
                .unwrap(),
            vec![
                hir::Literal::Integer(1234),
                hir::Literal::Integer(4321),
                hir::Literal::Integer(5678),
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
    fn literal_integer() {
        let filename = diag::FileName::new("literal_integer");
        let inputs = &[
            ("-1234", -1234),
            ("1234", 1234),
            ("-1_000_000", -1_000_000),
            ("1_000_000", 1_000_000),
            ("1_2__3___", 123),
            ("-0x1024", -4132),
            ("0x1024", 4132),
            ("-0x1_024", -4132),
            ("0x10___24", 4132),
            ("-0o033", -27),
            ("0o033", 27),
            ("-0o0_3_3", -27),
            ("0o03_3___", 27),
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
                    .expect(&mut LitInteger)
                    .expect("valid integer"),
                hir::Literal::Integer(*expected)
            );
        }

        let input: diag::Text = "1234 4321".into();
        let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
        assert_eq!(
            Parser::new(filename, &input, &tokens)
                .parse_until_eof(&mut LitInteger)
                .unwrap(),
            vec![hir::Literal::Integer(1234), hir::Literal::Integer(4321)]
        );

        for bad in &[
            "_1234", "0x_1234", "0xGG", "0b_1", "0b77", "0o_1", "0o9", "0f1234", "0X1234", "1.234",
            "45.56",
        ] {
            let bad: diag::Text = (*bad).into();
            let tokens = Lexer::new(&bad, filename).into_iter().collect::<Vec<_>>();

            Parser::new(filename, &bad, &tokens)
                .expect(&mut LitInteger)
                .unwrap_err();
        }
    }

    #[test]
    fn literal_float() {
        let filename = diag::FileName::new("literal_float");

        let inputs = &[
            ("3.14", 3.14),
            ("-3.14", -3.14),
            ("2.5E10", 2.5E10),
            ("2.5e10", 2.5e10),
            ("2.5E-10", 2.5E-10),
            ("5.", 5.),
            (".5", 0.5), // this should be an error
            ("0.5", 0.5),
            ("1.0e+1-1", 10.0),
            ("inf", std::f64::INFINITY),
            ("-inf", std::f64::NEG_INFINITY),
            ("NaN", std::f64::NAN),
        ];

        for (input, expected) in inputs {
            let input: diag::Text = (*input).into();
            log::info!("{}", input);
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

        let bad = &[
            "_1.1", "1._1", "._1", "1.e1", ".e1", "1.+1", ".+1", "1.1e/1",
        ];
        for bad in bad {
            let input: diag::Text = (*bad).into();
            let tokens = Lexer::new(&input, filename).into_iter().collect::<Vec<_>>();
            Parser::new(filename, &input, &tokens)
                .expect(&mut LitFloat)
                .unwrap_err();
        }
    }
}

