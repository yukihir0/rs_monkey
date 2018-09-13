pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let(Identifier),
    Return(Expression),
    Expression(Expression),
}

#[derive(PartialEq, Debug)]
pub struct Identifier(pub String);

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Integer(i64),
}

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}
