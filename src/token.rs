#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifier + Literal
    Identifier(String),
    Integer(String),
    String(String),

    // Operator
    Assign,      // =
    Bang,        // !
    Plus,        // +
    Minus,       // -
    Asterisk,    // *
    Slash,       // /
    LessThan,    // <
    GreaterThan, // >
    Equal,       // ==
    NotEqual,    // !=

    // Delimiters
    Comma,       // ,
    Semicolon,   // ;
    LeftParen,   // (
    RightParen,  // )
    LeftBrace,   // {
    RightBrace,  // }
    LeftBracket, // [
    RightBracket,// ]

    // Keywords
    Function,    // fn
    Let,         // let
    True,        // true
    False,       // false
    If,          // if
    Else,        // else
    Return,      // return
}

pub fn lookup_identifier(identifier: &str) -> Token {
    match identifier {
        "fn"     => Token::Function,
        "let"    => Token::Let,
        "true"   => Token::True,
        "false"  => Token::False,
        "if"     => Token::If,
        "else"   => Token::Else,
        "return" => Token::Return,
        _        => Token::Identifier(identifier.to_string()),
    }
}
