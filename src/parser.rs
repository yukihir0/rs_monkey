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

    fn token_to_precedence(tok: &Token) -> Precedence {
        match tok {
            Token::EQ | Token::NotEQ => Precedence::Equals,
            Token::LT | Token::GT => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
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

    fn error_no_prefix_parser(&mut self) {
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken,
            format!(
                "no prefix parse function for  \"{:?}\" found",
                self.current_token,
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

    fn current_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.current_token)
    }

    fn peek_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.peek_token)
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
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(name, expr))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => {
                if self.peek_token_is(&Token::Semicolon) {
                    self.next_token();
                }
                Some(Statement::Expression(expr))
            }
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // prefix
        let mut left = match self.current_token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Integer(_) => self.parse_integer_expression(),
            Token::True => self.parse_bool_expression(),
            Token::False => self.parse_bool_expression(),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expression(),
            Token::LParen => self.parse_grouped_expression(),
            _ => {
                self.error_no_prefix_parser();
                return None;
            }
        };

        // infix
        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_token_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::EQ
                | Token::NotEQ
                | Token::LT
                | Token::GT => {
                    self.next_token();
                    left = self.parse_infix_expression(left.unwrap());
                }
                _ => return left,
            }
        }

        left
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.current_token {
            Token::Identifier(ref mut identifier) => Some(Identifier(identifier.clone())),
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match self.parse_identifier() {
            Some(ident) => Some(Expression::Identifier(ident)),
            None => None,
        }
    }

    fn parse_integer_expression(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::Integer(ref mut num) => {
                let n = num.parse::<i64>().unwrap();
                Some(Expression::Literal(Literal::Integer(n)))
            },
            _ => None,
        }
    }

    fn parse_bool_expression(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::True => Some(Expression::Literal(Literal::Bool(true))),
            Token::False=> Some(Expression::Literal(Literal::Bool(false))),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = match self.current_token {
            Token::Bang => Prefix::Not,
            Token::Minus => Prefix::Minus,
            Token::Plus => Prefix::Plus,
            _ => return None,
        };

        self.next_token();

        match self.parse_expression(Precedence::Prefix) {
            Some(expr) => Some(Expression::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Divide,
            Token::Asterisk => Infix::Multiply,
            Token::EQ => Infix::Equal,
            Token::NotEQ => Infix::NotEqual,
            Token::LT => Infix::LessThan,
            Token::GT => Infix::GreaterThan,
            _ => return None,
        };

        let precedence = self.current_token_precedence();

        self.next_token();

        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Infix(infix, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek_token(Token::RParen) {
            None
        } else {
            expr
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use lexer::Lexer;
    use parser::Parser;

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
    fn test_let_statement() {
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
                Statement::Let(Identifier(String::from("x")), Expression::Literal(Literal::Integer(5))),
                Statement::Let(Identifier(String::from("y")), Expression::Literal(Literal::Integer(10))),
                Statement::Let(
                    Identifier(String::from("foobar")),
                    Expression::Literal(Literal::Integer(838383)),
                ),
            ],
            program,
        );
    }

    #[test]
    fn test_return_statement() {
        let input = r#"
    return 5;
    return 10;
    return 993322;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Statement::Return(Expression::Literal(Literal::Integer(5))),
                Statement::Return(Expression::Literal(Literal::Integer(10))),
                Statement::Return(Expression::Literal(Literal::Integer(993322))),
            ],
            program,
        );
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Identifier(Identifier(String::from("foobar"))))],
            program,
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(vec![Statement::Expression(Expression::Literal(Literal::Integer(5)))], program,);
    }

    #[test]
    fn test_boolean_literal_expr() {
        let tests = vec![
            ("true;", Statement::Expression(Expression::Literal(Literal::Bool(true)))),
            ("false;", Statement::Expression(Expression::Literal(Literal::Bool(false)))),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_prefix_expression() {
        let tests = vec![
            (
                "!5;",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "-15;",
                Statement::Expression(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(Expression::Literal(Literal::Integer(15))),
                )),
            ),
            (
                "+15;",
                Statement::Expression(Expression::Prefix(
                    Prefix::Plus,
                    Box::new(Expression::Literal(Literal::Integer(15))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            (
                "5 + 5;",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 - 5;",
                Statement::Expression(Expression::Infix(
                    Infix::Minus,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 * 5;",
                Statement::Expression(Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 / 5;",
                Statement::Expression(Expression::Infix(
                    Infix::Divide,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 > 5;",
                Statement::Expression(Expression::Infix(
                    Infix::GreaterThan,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 < 5;",
                Statement::Expression(Expression::Infix(
                    Infix::LessThan,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 == 5;",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
            (
                "5 != 5;",
                Statement::Expression(Expression::Infix(
                    Infix::NotEqual,
                    Box::new(Expression::Literal(Literal::Integer(5))),
                    Box::new(Expression::Literal(Literal::Integer(5))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }

     #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            (
                "-a * b",
                Statement::Expression(Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("b")))),
                )),
            ),
            (
                "!-a",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                )),
            ),
            (
                "a + b + c",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b - c",
                Statement::Expression(Expression::Infix(
                    Infix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b * c",
                Statement::Expression(Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Infix(
                        Infix::Multiply,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b / c",
                Statement::Expression(Expression::Infix(
                    Infix::Divide,
                    Box::new(Expression::Infix(
                        Infix::Multiply,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b / c",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    Box::new(Expression::Infix(
                        Infix::Divide,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                        Box::new(Expression::Identifier(Identifier(String::from("c")))),
                    )),
                )),
            ),
            (
                "a + b * c + d / e - f",
                Statement::Expression(Expression::Infix(
                    Infix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Identifier(Identifier(String::from("a")))),
                            Box::new(Expression::Infix(
                                Infix::Multiply,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )),
                        )),
                        Box::new(Expression::Infix(
                            Infix::Divide,
                            Box::new(Expression::Identifier(Identifier(String::from("d")))),
                            Box::new(Expression::Identifier(Identifier(String::from("e")))),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("f")))),
                )),
            ),
            (
                "5 > 4 == 3 < 4",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                    Box::new(Expression::Infix(
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                )),
            ),
            (
                "5 < 4 != 3 > 4",
                Statement::Expression(Expression::Infix(
                    Infix::NotEqual,
                    Box::new(Expression::Infix(
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                    Box::new(Expression::Infix(
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                )),
            ),
            // TODO >= <=
            /*
            (
                "5 >= 4 == 3 <= 4",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::GreaterThanEqual,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                    Box::new(Expression::Infix(
                        Infix::LessThanEqual,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Literal(Literal::Integer(4))),
                    )),
                )),
            ),
            (
                "5 <= 4 != 3 >= 4",
                Stmt::Expr(Expr::Infix(
                    Infix::NotEqual,
                    Box::new(Expr::Infix(
                        Infix::LessThanEqual,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::GreaterThanEqual,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            */
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(4))),
                            Box::new(Expression::Literal(Literal::Integer(5))),
                        )),
                    )),
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(3))),
                            Box::new(Expression::Literal(Literal::Integer(1))),
                        )),
                        Box::new(Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(4))),
                            Box::new(Expression::Literal(Literal::Integer(5))),
                        )),
                    )),
                )),
            ),
            (
                "true",
                Statement::Expression(Expression::Literal(Literal::Bool(true)))
            ),
            (
                "false",
                Statement::Expression(Expression::Literal(Literal::Bool(false)))
            ),
            (
                "3 > 5 == false",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Bool(false))),
                )),
            ),
            (
                "3 < 5 == true",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::LessThan,
                        Box::new(Expression::Literal(Literal::Integer(3))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Bool(true))),
                )),
            ),
            (
                "1 + (2 + 3) + 4",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(1))),
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Integer(2))),
                            Box::new(Expression::Literal(Literal::Integer(3))),
                        )),
                    )),
                    Box::new(Expression::Literal(Literal::Integer(4))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Statement::Expression(Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Statement::Expression(Expression::Infix(
                    Infix::Divide,
                    Box::new(Expression::Literal(Literal::Integer(2))),
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    )),
                )),
            ),
            (
                "-(5 + 5)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(5))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    )),
                )),
            ),
            (
                "!(true == true)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Not,
                    Box::new(Expression::Infix(
                        Infix::Equal,
                        Box::new(Expression::Literal(Literal::Bool(true))),
                        Box::new(Expression::Literal(Literal::Bool(true))),
                    )),
                )),
            ),
            /*
            (
                "a + add(b * c) + d",
                Stmt::Expr(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Call {
                            func: Box::new(Expr::Ident(Ident(String::from("add")))),
                            args: vec![Expr::Infix(
                                Infix::Multiply,
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Box::new(Expr::Ident(Ident(String::from("c")))),
                            )],
                        }),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("d")))),
                )),
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                Stmt::Expr(Expr::Call {
                    func: Box::new(Expr::Ident(Ident(String::from("add")))),
                    args: vec![
                        Expr::Ident(Ident(String::from("a"))),
                        Expr::Ident(Ident(String::from("b"))),
                        Expr::Literal(Literal::Int(1)),
                        Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::Int(2))),
                            Box::new(Expr::Literal(Literal::Int(3))),
                        ),
                        Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Literal(Literal::Int(4))),
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                        Expr::Call {
                            func: Box::new(Expr::Ident(Ident(String::from("add")))),
                            args: vec![
                                Expr::Literal(Literal::Int(6)),
                                Expr::Infix(
                                    Infix::Multiply,
                                    Box::new(Expr::Literal(Literal::Int(7))),
                                    Box::new(Expr::Literal(Literal::Int(8))),
                                ),
                            ],
                        },
                    ],
                }),
            ),
            (
                "add(a + b + c * d / f + g)",
                Stmt::Expr(Expr::Call {
                    func: Box::new(Expr::Ident(Ident(String::from("add")))),
                    args: vec![Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Infix(
                                Infix::Plus,
                                Box::new(Expr::Ident(Ident(String::from("a")))),
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                            )),
                            Box::new(Expr::Infix(
                                Infix::Divide,
                                Box::new(Expr::Infix(
                                    Infix::Multiply,
                                    Box::new(Expr::Ident(Ident(String::from("c")))),
                                    Box::new(Expr::Ident(Ident(String::from("d")))),
                                )),
                                Box::new(Expr::Ident(Ident(String::from("f")))),
                            )),
                        )),
                        Box::new(Expr::Ident(Ident(String::from("g")))),
                    )],
                }),
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                Stmt::Expr(Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Infix(
                        Infix::Multiply,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Index(
                            Box::new(Expr::Literal(Literal::Array(vec![
                                Expr::Literal(Literal::Int(1)),
                                Expr::Literal(Literal::Int(2)),
                                Expr::Literal(Literal::Int(3)),
                                Expr::Literal(Literal::Int(4)),
                            ]))),
                            Box::new(Expr::Infix(
                                Infix::Multiply,
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Box::new(Expr::Ident(Ident(String::from("c")))),
                            )),
                        )),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("d")))),
                )),
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                Stmt::Expr(Expr::Call {
                    func: Box::new(Expr::Ident(Ident(String::from("add")))),
                    args: vec![
                        Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Ident(Ident(String::from("a")))),
                            Box::new(Expr::Index(
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Box::new(Expr::Literal(Literal::Int(2))),
                            )),
                        ),
                        Expr::Index(
                            Box::new(Expr::Ident(Ident(String::from("b")))),
                            Box::new(Expr::Literal(Literal::Int(1))),
                        ),
                        Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::Int(2))),
                            Box::new(Expr::Index(
                                Box::new(Expr::Literal(Literal::Array(vec![
                                    Expr::Literal(Literal::Int(1)),
                                    Expr::Literal(Literal::Int(2)),
                                ]))),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )),
                        ),
                    ],
                }),
            ),
            */
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }
}
