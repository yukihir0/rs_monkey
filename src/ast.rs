pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let(Identifier),
}

#[derive(PartialEq, Debug)]
pub struct Identifier(pub String);
