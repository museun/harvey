#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub expressions: Vec<Expression>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Function {
        identifier: Identifier,
        initializer: Option<Box<Self>>,
        body: Box<Self>,
    },
    Let {
        variable: Variable,
        expression: Box<Self>,
    },
    Call {
        function: Box<Self>,
        arguments: Vec<Self>,
    },
    Match {
        expression: Box<Self>,
        arms: Vec<MatchArm>,
    },
    Lambda {
        variables: Vec<Variable>,
        body: Box<Self>,
    },
    Unit {},
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub match_: Expression, // TODO
    pub guard: Expression,  // TODO
    pub body: Expression,   // TODO
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Import(Import),
    Const(Const),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Record(Record),
    Enum(Enum),
    Union(Union),
    Alias(Alias),
    Tuple(Tuple), // this should be implicit
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub identifier: Identifier,
    pub generics: Vec<Identifier>,
    pub fields: Vec<(Identifier, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub identifiers: Identifier,
    pub generics: Vec<Identifier>,
    pub variants: Vec<(Identifier, Variable)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub identifier: Identifier,
    pub types: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias {
    pub left: Identifier,
    pub right: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub elements: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub paths: Vec<Vec<Path>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Const {
    pub identifier: Identifier,
    pub literal: Literal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    // TODO narrow these
    Float(f64),
    Integer(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    Ident(Identifier),
    Glob,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Directive {
    pub directive: Identifier,
}
