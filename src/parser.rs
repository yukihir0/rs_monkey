use token::Token;
use ast::*;
use lexer::Lexer;
use std::fmt;

pub type ParseErrors = Vec<ParseError>;

#[derive(Clone, Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    msg: String,
}

impl ParseError {
    fn new(kind: ParseErrorKind, msg: String) -> Self {
        ParseError { kind, msg }
    }
}

#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    UnexpectedToken,
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseErrorKind::UnexpectedToken => write!(f, "Unexpected Token"),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: ParseErrors,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, tok: Token) -> bool {
        self.current_token == tok
    }

    fn peek_token_is(&mut self, tok: &Token) -> bool {
        self.peek_token == *tok
    }

    fn error_peek_token(&mut self, tok: Token) {
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken,
            format!(
                "expected next token to be {:?}, got {:?} instead",
                tok, self.peek_token
            ),
        ));
    }

    fn expect_peek_token(&mut self, tok: Token) -> bool {
        if self.peek_token_is(&tok) {
            self.next_token();
            return true;
        } else {
            self.error_peek_token(tok);
            return false;
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program: Program = vec![];

        while !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Some(stmt) => program.push(stmt), None => {}
            }
            self.next_token();
        }

        program
    }


    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match &self.peek_token {
            Token::Identifier(_) => self.next_token(),
            _ => return None,
        };

        let name = match self.parse_identifier() {
            Some(name) => name,
            None => return None,
        };

        if !self.expect_peek_token(Token::Assign) {
            return None;
        }

        self.next_token();

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(name))
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.current_token {
            Token::Identifier(ref mut identifier) => Some(Identifier(identifier.clone())),
            _ => None,
        }
    }
}

fn check_parse_errors(parser: &mut Parser) {
    let errors = parser.errors.clone();

    println!("parser has {} errors", errors.len());
    if errors.len() == 0 {
        return;
    }

    println!("\n");

    println!("parser has {} errors", errors.len());

    for err in errors {
        println!("parse error: {:?}", err);
    }

    println!("\n");

    panic!("failed");
}

#[test]
fn test_let_stmt() {
    let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
    "#;

    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse();

    check_parse_errors(&mut parser);
    assert_eq!(
        vec![
            Statement::Let(Identifier(String::from("x"))),
            Statement::Let(Identifier(String::from("y"))),
            Statement::Let(Identifier(String::from("foobar"))),
        ],
        program,
    );
}
