pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

pub type BlockStatement = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub struct Identifier(pub String);

#[derive(PartialEq, Debug)]
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
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Integer(i64),
    Bool(bool),
}

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, PartialOrd, Debug)]
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
