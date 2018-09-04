// token.rs
#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifier + Literal
    Identifier(String),
    Integer(String),

    // Operator
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}

// lexer.rs
use std::str::Chars;
use std::iter::Peekable;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    pub fn next_token(&mut self) -> Token {
        match self.read_char() {
            Some('=') => Token::Assign,
            Some('+') => Token::Plus,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some(_) => Token::Illegal,
            None => Token::EOF,
        }
    }
}

#[test]
fn next_token_test() {
    let input = "=+(){},;";

    let expects = vec![
        Token::Assign,
        Token::Plus,
        Token::LParen,
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
        Token::Comma,
        Token::Semicolon,
    ];

    let mut l = Lexer::new(input);
    for e in expects {
        let t = l.next_token();
        assert_eq!(t, e);
    }
}

fn main() {
    println!("Hello, world!");
}
