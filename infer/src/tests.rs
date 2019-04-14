use super::*;

#[test]
fn string() {
    synth::<ty::Literal>(
        literal("foobar"), //
        Type::Literal(ty::LiteralType::String),
    );
}

#[test]
fn apply_string() {
    synth::<ty::Literal>(
        apply(decl("x", var("x")), literal("foobar")),
        Type::Literal(ty::LiteralType::String),
    );
}

#[test]
fn apply_integer() {
    synth::<ty::Literal>(
        apply(decl("x", var("x")), literal(42)),
        Type::Literal(ty::LiteralType::Integer),
    );
}

#[test]
fn lambda() {
    synth::<ty::Literal>(
        decl("x", var("x")),
        Type::Function(
            existential("t0").into(), //
            existential("t0").into(),
        ),
    );
}

#[test]
fn unit() {
    synth::<ty::Literal>(
        id(literal(42)), //
        Type::Literal(ty::LiteralType::Integer),
    );
}

#[test]
fn tuple() {
    synth::<ty::Literal>(
        super::tuple(literal("foobar"), literal(true)), //
        prod(ty::LiteralType::String, ty::LiteralType::Bool),
    );
}

#[test]
fn tuple_apply() {
    synth::<ty::Literal>(
        apply(
            decl("a", super::tuple(var("a"), var("a"))),
            literal("foobar"),
        ),
        prod(ty::LiteralType::String, ty::LiteralType::String),
    );
}

#[test]
fn tuple_apply_2() {
    synth::<ty::Literal>(
        apply(
            decl(
                "a",
                super::tuple(var("a"), super::tuple(var("a"), var("a"))),
            ),
            literal("foobar"),
        ),
        Type::Product(
            Type::Literal(ty::LiteralType::String).into(), //
            prod(ty::LiteralType::String, ty::LiteralType::String).into(),
        ),
    );
}

#[test]
fn tuple_product() {
    synth::<ty::Literal>(
        id(super::tuple(literal("foobar"), literal(42))), //
        prod(ty::LiteralType::String, ty::LiteralType::Integer),
    );
}

#[test]
fn let_() {
    synth::<ty::Literal>(
        super::let_(
            "a",
            literal(42),
            id(var("a")), //
        ),
        Type::Literal(ty::LiteralType::Integer),
    );
}

#[test]
fn let_apply() {
    synth::<ty::Literal>(
        apply(
            super::let_("foo", decl("x", var("x")), var("foo")),
            literal("foobar"),
        ),
        Type::Literal(ty::LiteralType::String),
    );
}

#[test]
fn let_product() {
    synth::<ty::Literal>(
        super::let_(
            "foo",
            id_anno(),
            super::tuple(
                apply(var("foo"), literal("foobar")),
                apply(var("foo"), literal(true)),
            ),
        ),
        prod(ty::LiteralType::String, ty::LiteralType::Bool),
    );
}

fn init_logger() {
    let _ = env_logger::builder()
        .default_format_timestamp(false)
        .try_init();
}

fn synth<Lit: Literal>(expr: Expression<Lit>, ty: Type<Lit>) {
    init_logger();
    assert_eq!(expr.synthesize(&mut State::default()).unwrap(), ty)
}

fn make_str(s: impl Into<RcString>) -> RcString {
    s.into()
}

fn existential<Lit: Literal>(s: impl Into<RcString>) -> Type<Lit> {
    Type::Existential(s.into())
}

fn quant<Lit: Literal>(s: impl Into<RcString>, ty: Type<Lit>) -> Type<Lit> {
    Type::Quantification(s.into(), Box::new(ty))
}

fn id_anno<Lit: Literal>() -> Expression<Lit> {
    anno(
        decl("x", var("x")),
        quant(
            "t",
            Type::Function(
                Type::Var(make_str("t")).into(), //
                Type::Var(make_str("t")).into(),
            ),
        ),
    )
}

fn id<Lit: Literal>(expr: Expression<Lit>) -> Expression<Lit> {
    apply(id_anno(), expr)
}

fn prod(l: ty::LiteralType, r: ty::LiteralType) -> Type<ty::Literal> {
    Type::Product(Type::Literal(l).into(), Type::Literal(r).into())
}


mod ty {
    #[derive(Clone, Debug, PartialEq)]
    pub enum Literal {
        Char(char),
        String(String),
        Integer(i64),
        Float(f64),
        Bool(bool),
        Unit,
    }

    impl crate::Literal for Literal {
        type Type = LiteralType;
        fn synthesize(&self) -> Self::Type {
            use Literal::*;
            match self {
                Char(..) => LiteralType::Char,
                String(..) => LiteralType::String,
                Integer(..) => LiteralType::Integer,
                Float(..) => LiteralType::Float,
                Bool(..) => LiteralType::Bool,
                Unit => LiteralType::Unit,
            }
        }

        fn check(&self, ty: &Self::Type) -> bool {
            match (self, ty) {
                (Literal::Char(..), LiteralType::Char) => true,
                (Literal::String(..), LiteralType::String) => true,
                (Literal::Integer(..), LiteralType::Integer) => true,
                (Literal::Float(..), LiteralType::Float) => true,
                (Literal::Bool(..), LiteralType::Bool) => true,
                (Literal::Unit, LiteralType::Unit) => true,
                _ => false,
            }
        }
    }

    impl std::fmt::Display for Literal {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Literal::*;
            match self {
                Char(ch) => write!(f, "'{}'", ch),
                String(s) => write!(f, "\"{}\"", s),
                Integer(i) => write!(f, "{}", i),
                Float(d) => write!(f, "{}", d),
                Bool(b) => write!(f, "{}", b),
                Unit => write!(f, "()"),
            }
        }
    }

    impl From<char> for Literal {
        fn from(ch: char) -> Self {
            Literal::Char(ch)
        }
    }
    impl From<String> for Literal {
        fn from(s: String) -> Self {
            Literal::String(s)
        }
    }

    impl From<&str> for Literal {
        fn from(s: &str) -> Self {
            Literal::String(s.into())
        }
    }

    impl From<i64> for Literal {
        fn from(d: i64) -> Self {
            Literal::Integer(d)
        }
    }
    impl From<f64> for Literal {
        fn from(d: f64) -> Self {
            Literal::Float(d)
        }
    }
    impl From<bool> for Literal {
        fn from(b: bool) -> Self {
            Literal::Bool(b)
        }
    }
    impl From<()> for Literal {
        fn from(_: ()) -> Self {
            Literal::Unit
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum LiteralType {
        Char,
        String,
        Integer,
        Float,
        Bool,
        Unit,
    }

    impl std::fmt::Display for LiteralType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use LiteralType::*;
            match self {
                Char => write!(f, "Char"),
                String => write!(f, "String"),
                Integer => write!(f, "Integer"),
                Float => write!(f, "Float"),
                Bool => write!(f, "Bool"),
                Unit => write!(f, "()"),
            }
        }
    }
}
