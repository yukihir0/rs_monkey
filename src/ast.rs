use std::fmt;

pub type Program = Vec<Statement>;

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

pub type BlockStatement = Vec<Statement>;

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        condition:   Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        params: Vec<Identifier>,
        body:   BlockStatement,
    },
    Call {
        function: Box<Expression>,
        args:     Vec<Expression>,
    },
    Index(Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Bool(bool),
    String(String),
    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Infix::Plus        => write!(f, "+"),
            Infix::Minus       => write!(f, "-"),
            Infix::Multiply    => write!(f, "*"),
            Infix::Divide      => write!(f, "/"),
            Infix::Equal       => write!(f, "=="),
            Infix::NotEqual    => write!(f, "!="),
            Infix::LessThan    => write!(f, "<"),
            Infix::GreaterThan => write!(f, ">"),
        }
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Precedence {
    Lowest,
    Equals,      // == or !=
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}
