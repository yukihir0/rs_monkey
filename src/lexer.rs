use token;
use token::Token;

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

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn peek_is_letter(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => is_letter(ch),
            None => false,
        }
    }

    fn peek_char_eq(&mut self, ch: char) -> bool {
        match self.peek_char() {
            Some(&peek_ch) => peek_ch == ch,
            None => false,
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);

        while self.peek_is_letter() {
            ident.push(self.read_char().unwrap());
        }

        ident
    }

    fn read_number(&mut self, first: char) -> String {
        let mut number = String::new();
        number.push(first);

        while let Some(&c) = self.peek_char() {
            if !c.is_numeric() {
                break;
            }
            number.push(self.read_char().unwrap());
        }

        number
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        
        match self.read_char() {
            Some('=') => {
                if self.peek_char_eq('=') {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::Assign
                }
            },
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => {
                if self.peek_char_eq('=') {
                    self.read_char();
                    Token::NotEQ
                } else {
                    Token::Bang
                }
            },
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some(ch @ _) => {
                if is_letter(ch) {
                    let literal = self.read_identifier(ch);
                    token::lookup_ident(&literal)
                } else if ch.is_numeric() {
                    Token::Integer(self.read_number(ch))
                } else {
                    Token::Illegal
                }
            }
            None => Token::EOF,
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[test]
fn next_token_test() {
    let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
   return true;
} else {
   return false;
}

10 == 10;
10 != 9;
"#;

    let expects = vec![
        Token::Let,
        Token::Identifier("five".to_string()),
        Token::Assign,
        Token::Integer("5".to_string()),
        Token::Semicolon,
        Token::Let,
        Token::Identifier("ten".to_string()),
        Token::Assign,
        Token::Integer("10".to_string()),
        Token::Semicolon,
        Token::Let,
        Token::Identifier("add".to_string()),
        Token::Assign,
        Token::Function,
        Token::LParen,
        Token::Identifier("x".to_string()),
        Token::Comma,
        Token::Identifier("y".to_string()),
        Token::RParen,
        Token::LBrace,
        Token::Identifier("x".to_string()),
        Token::Plus,
        Token::Identifier("y".to_string()),
        Token::Semicolon,
        Token::RBrace,
        Token::Semicolon,
        Token::Let,
        Token::Identifier("result".to_string()),
        Token::Assign,
        Token::Identifier("add".to_string()),
        Token::LParen,
        Token::Identifier("five".to_string()),
        Token::Comma,
        Token::Identifier("ten".to_string()),
        Token::RParen,
        Token::Semicolon,
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Integer("5".to_string()),
        Token::Semicolon,
        Token::Integer("5".to_string()),
        Token::LT,
        Token::Integer("10".to_string()),
        Token::GT,
        Token::Integer("5".to_string()),
        Token::Semicolon,
        Token::If,
        Token::LParen,
        Token::Integer("5".to_string()),
        Token::LT,
        Token::Integer("10".to_string()),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RBrace,
        Token::Else,
        Token::LBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RBrace,
        Token::Integer("10".to_string()),
        Token::EQ,
        Token::Integer("10".to_string()),
        Token::Semicolon,
        Token::Integer("10".to_string()),
        Token::NotEQ,
        Token::Integer("9".to_string()),
        Token::Semicolon,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for e in expects {
        let t = l.next_token();
        assert_eq!(t, e);
    }
}

