use super::*;

// type Person = { name : String, dead : Bool }
// type Dog[T] = { kind  : T
//                 color : Color }

// type Maybe[T] = Just T | Nothing

// type Union = Start | Stop | Panic

// type Variant[T] = Int
//                 | Foo{ kind : T }
//                 | Bar(Bool)
//                 | Int * Int * Int

// const PI = 3.2 // ish

// type Maybe = Option

#[derive(Debug)]
pub struct Type;
impl<'a> Syntax<'a> for Type {
    type Output = hir::Type;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Keyword::Type)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Keyword::Type)?;
        let identifier = parser.expect(&mut Identifier)?;
        let generics = if parser.test(&mut Generics) {
            parser.expect(&mut Generics)?
        } else {
            vec![]
        };
        dbg!(parser.expect(&mut Sigil::Equal))?;
        dbg!(parser.skip(Token::Whitespace));

        let mut identifier = Some(identifier);
        macro_rules! try_it {
            ($parser:expr=> $ty:expr) => {{
                let mut tx = parser.checkpoint();
                if let Ok(ty) = tx.expect(&mut $parser).map($ty) {
                    tx.commit();
                    return Ok(ty);
                };
                drop(tx);
                std::mem::replace(&mut identifier, $parser.0.take());
            }};
        }

        if parser.is(Sigil::OpenBrace) {
            return parser
                .expect(&mut Record(identifier, Some(generics)))
                .map(hir::Type::Record);
        }

        let mut alias = Alias(identifier.take());
        try_it!(alias=> hir::Type::Alias);

        let mut union = Union(identifier.take());
        try_it!(union=> hir::Type::Union);

        let mut enum_ = Enum(identifier.take(), Some(generics));
        try_it!(enum_=> hir::Type::Enum);

        Err(parser.report_error_current("invalid type definition"))
    }
}

#[derive(Debug)]
struct Record(Option<hir::Identifier>, Option<Vec<hir::Identifier>>);
impl<'a> Syntax<'a> for Record {
    type Output = hir::Record;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::OpenBrace)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let (identifier, generics) = (self.0.take().expect("id"), self.1.take().expect("generics"));
        parser
            .expect(&mut Surround::new(
                Then(Sigil::OpenBrace, Sink(Token::Whitespace)),
                Fields(Guard(Sigil::Colon, Identifier)),
                Then(Sigil::CloseBrace, Sink(Token::Whitespace)),
            ))
            .map(|fields| hir::Record {
                identifier,
                generics,
                fields,
            })
    }
}

#[derive(Debug)]
struct Enum(Option<hir::Identifier>, Option<Vec<hir::Identifier>>);
impl<'a> Syntax<'a> for Enum {
    type Output = hir::Enum;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let (identifier, generics) = (self.0.take().unwrap(), self.1.take().unwrap());
        parser
            .expect(&mut Surround::new(
                Sigil::OpenBrace,
                Fields(Variable),
                Sigil::CloseBrace,
            ))
            .map(|variants| hir::Enum {
                identifier,
                generics,
                variants,
            })
    }
}

#[derive(Debug)]
struct Union(Option<hir::Identifier>);
impl<'a> Syntax<'a> for Union {
    type Output = hir::Union;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let identifier = self.0.take().unwrap();
        parser
            .expect(&mut List::new(Identifier, Sigil::Pipe))
            .map(|types| hir::Union { identifier, types })
    }
}

#[derive(Debug)]
struct Alias(Option<hir::Identifier>);
impl<'a> Syntax<'a> for Alias {
    type Output = hir::Alias;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        let left = self.0.take().unwrap();
        parser
            .expect(&mut Identifier)
            .map(|right| hir::Alias { left, right })
    }
}

#[derive(Debug)]
pub struct Generics;
impl<'a> Syntax<'a> for Generics {
    type Output = Vec<hir::Identifier>;
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Sigil::OpenBracket)
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser.expect(&mut Surround(
            Sigil::OpenBracket,
            List::new(Identifier, Sigil::Comma),
            Sigil::CloseBracket,
        ))
    }
}

type HirField<T> = (hir::Identifier, T);

#[derive(Debug)]
struct Field<'b, T>(pub &'b mut T);
impl<'a, 'b, T> Syntax<'a> for Field<'b, T>
where
    T: Syntax<'a>,
{
    type Output = HirField<T::Output>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut Identifier)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        parser
            .expect(&mut Identifier)
            .and_then(|id| Ok((id, parser.expect(&mut self.0)?)))
    }
}

#[derive(Debug)]
struct Fields<T>(pub T);
impl<'a, T> Syntax<'a> for Fields<T>
where
    T: Syntax<'a> + std::fmt::Debug,
    T::Output: std::fmt::Debug,
{
    type Output = Vec<HirField<T::Output>>;

    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.test(&mut self.0)
    }

    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        dbg!(parser.expect(&mut List::new(Field(&mut self.0), EndOfLine)))
    }
}

#[derive(Debug)]
struct EndOfLine;
impl<'a> Syntax<'a> for EndOfLine {
    type Output = ();
    fn test(&mut self, parser: &Parser<'a>) -> bool {
        parser.is(Token::NewLine) || parser.is(Sigil::Comma)
    }
    fn expect(&mut self, parser: &mut Parser<'a>) -> Result<Self::Output> {
        if parser.is(Token::NewLine) {
            return parser.expect(&mut Token::NewLine).map(|_| ());
        }
        parser.expect(&mut Sigil::Comma).map(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record() {
        let _ = env_logger::builder()
            .default_format_timestamp(false)
            .try_init();

        let filename = diag::FileName::new("record");
        let input: diag::Text = "
type Person = { name : String, dead : Bool }
type Dog[T] = { kind  : T
                color : Color }
"
        .into();

        let tokens = lexer::Lexer::new(&input, filename)
            .into_iter()
            .filter(|k| k.value != Token::Whitespace)
            .collect::<Vec<_>>();

        for ty in Parser::new(filename, &input, &tokens)
            .parse_until_eof(&mut Type)
            .expect("parse")
        {
            eprintln!("{:#?}", ty);
        }
    }

    #[test]
    #[ignore]
    fn enum_() {}

    #[test]
    #[ignore]
    fn union_() {}

    #[test]
    #[ignore]
    fn alias_() {}
}
