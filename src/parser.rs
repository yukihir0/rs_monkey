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

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
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

    fn token_to_precedence(token: &Token) -> Precedence {
        match token {
            Token::Equal
            | Token::NotEqual    => Precedence::Equals,
            Token::LessThan
            | Token::GreaterThan => Precedence::LessGreater,
            Token::Plus
            | Token::Minus       => Precedence::Sum,
            Token::Asterisk
            | Token::Slash       => Precedence::Product,
            Token::LeftBracket   => Precedence::Index,
            Token::LeftParen     => Precedence::Call,
            _                    => Precedence::Lowest,
        }
    }

    pub fn get_errors(&mut self) -> ParseErrors {
        self.errors.clone()
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        self.peek_token == *token
    }

    fn error_peek_token(&mut self, token: Token) {
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken,
            format!("expected next token to be {:?}, got {:?} instead", token, self.peek_token),
        ));
    }

    fn error_no_prefix_parser(&mut self) {
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken,
            format!("no prefix parse function for  \"{:?}\" found", self.current_token,),
        ));
    }

    fn expect_peek_token(&mut self, token: Token) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            return true;
        } else {
            self.error_peek_token(token);
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
                Some(statement) => program.push(statement),
                None            => {},
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let    => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _             => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match &self.peek_token {
            Token::Identifier(_) => self.next_token(),
            _                    => return None,
        };

        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None             => return None,
        };

        if !self.expect_peek_token(Token::Assign) {
            return None;
        }

        self.next_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None             => return None,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(identifier, expression))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None             => return None,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(expression))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expression) => {
                if self.peek_token_is(&Token::Semicolon) {
                    self.next_token();
                }
                Some(Statement::Expression(expression))
            }
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // prefix
        let mut left = match self.current_token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Integer(_)    => self.parse_integer_expression(),
            Token::True          => self.parse_bool_expression(),
            Token::False         => self.parse_bool_expression(),
            Token::String(_)     => self.parse_string_expression(),
            Token::LeftBracket   => self.parse_array_expression(),
            Token::Bang
            | Token::Plus
            | Token::Minus       => self.parse_prefix_expression(),
            Token::LeftParen     => self.parse_grouped_expression(),
            Token::If            => self.parse_if_expression(),
            Token::Function      => self.parse_function_expression(),
            _                    => {
                                        self.error_no_prefix_parser();
                                        return None;
                                    }
        };

        // infix
        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_token_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::GreaterThan => {
                    self.next_token();
                    left = self.parse_infix_expression(left.unwrap());
                }
                Token::LeftParen => {
                    self.next_token();
                    left = self.parse_call_expression(left.unwrap());
                }
                Token::LeftBracket => {
                    self.next_token();
                    left = self.parse_index_expression(left.unwrap());
                }
                _ => return left,
            }
        }

        left
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.current_token {
            Token::Identifier(ref mut identifier) => Some(Identifier(identifier.clone())),
            _                                     => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match self.parse_identifier() {
            Some(identifier) => Some(Expression::Identifier(identifier)),
            None             => None,
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
            Token::True  => Some(Expression::Literal(Literal::Bool(true))),
            Token::False => Some(Expression::Literal(Literal::Bool(false))),
            _            => None,
        }
    }

    fn parse_string_expression(&mut self) -> Option<Expression> {
        match self.current_token {
            Token::String(ref mut s) => Some(Expression::Literal(Literal::String(s.clone()))),
            _                        => None,
        }
    }

    fn parse_array_expression(&mut self) -> Option<Expression> {
        match self.parse_expression_list(Token::RightBracket) {
            Some(list) => Some(Expression::Literal(Literal::Array(list))),
            None => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = match self.current_token {
            Token::Bang  => Prefix::Not,
            Token::Minus => Prefix::Minus,
            Token::Plus  => Prefix::Plus,
            _            => return None,
        };

        self.next_token();

        match self.parse_expression(Precedence::Prefix) {
            Some(expression) => Some(Expression::Prefix(prefix, Box::new(expression))),
            None             => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let infix = match self.current_token {
            Token::Plus        => Infix::Plus,
            Token::Minus       => Infix::Minus,
            Token::Asterisk    => Infix::Multiply,
            Token::Slash       => Infix::Divide,
            Token::Equal       => Infix::Equal,
            Token::NotEqual    => Infix::NotEqual,
            Token::LessThan    => Infix::LessThan,
            Token::GreaterThan => Infix::GreaterThan,
            _                  => return None,
        };

        let precedence = self.current_token_precedence();

        self.next_token();

        match self.parse_expression(precedence) {
            Some(expression) => Some(Expression::Infix(infix, Box::new(left), Box::new(expression))),
            None             => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek_token(Token::RightParen) {
            None
        } else {
            expression
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek_token(Token::LeftParen) {
            return None;
        }

        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None             => return None,
        };

        if !self.expect_peek_token(Token::RightParen) || !self.expect_peek_token(Token::LeftBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative = None;

        if self.peek_token_is(&Token::Else) {
            self.next_token();

            if !self.expect_peek_token(Token::LeftBrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If {
            condition: Box::new(condition),
            consequence: consequence,
            alternative: alternative,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        self.next_token();

        let mut block = vec![];

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Some(statement) => block.push(statement),
                None            => {}
            }
            self.next_token();
        }

        block
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek_token(Token::LeftParen) {
            return None;
        }

        let params = match self.parse_function_params() {
            Some(params) => params,
            None         => return None,
        };

        if !self.expect_peek_token(Token::LeftBrace) {
            return None;
        }

        Some(Expression::Function {
            params: params,
            body:   self.parse_block_statement(),
        })
    }

    fn parse_function_params(&mut self) -> Option<Vec<Identifier>> {
        let mut params = vec![];

        if self.peek_token_is(&Token::RightParen) {
            self.next_token();
            return Some(params);
        }

        self.next_token();

        match self.parse_identifier() {
            Some(identifier) => params.push(identifier),
            None             => return None,
        };

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_identifier() {
                Some(identifier) => params.push(identifier),
                None             => return None,
            };
        }

        if !self.expect_peek_token(Token::RightParen) {
            return None;
        }

        Some(params)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let args = match self.parse_expression_list(Token::RightParen) {
            Some(args) => args,
            None       => return None,
        };

        Some(Expression::Call {
            function: Box::new(function),
            args:     args,
        })
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();

        match self.parse_expression(Precedence::Lowest) {
            Some(expression) => list.push(expression),
            None             => return None,
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_expression(Precedence::Lowest) {
                Some(expression) => list.push(expression),
                None             => return None,
            }
        }

        if !self.expect_peek_token(end) {
            return None;
        }

        Some(list)
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let index = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None             => return None,
        };

        if !self.expect_peek_token(Token::RightBracket) {
            return None;
        }

        Some(Expression::Index(Box::new(left), Box::new(index)))
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
    fn test_boolean_literal_expression() {
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
    fn test_string_literal_expression() {
        let input = "\"hello world\";";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Literal(Literal::String(String::from("hello world"))))],
            program,
        );
    }

    #[test]
    fn test_array_literal_expression() {
        let input = "[1, 2 * 2, 3 + 3]";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Literal(Literal::Array(vec![
                Expression::Literal(Literal::Integer(1)),
                Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Literal(Literal::Integer(2))),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                ),
                Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Integer(3))),
                    Box::new(Expression::Literal(Literal::Integer(3))),
                ),
            ])))],
            program,
        );
    }

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Index(
                Box::new(Expression::Identifier(Identifier(String::from("myArray")))),
                Box::new(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Integer(1))),
                    Box::new(Expression::Literal(Literal::Integer(1))),
                )),
            ))],
            program
        );
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
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix(
                    Infix::LessThan,
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                )),
                consequence: vec![Statement::Expression(Expression::Identifier(Identifier(String::from("x"))))],
                alternative: None,
            })],
            program,
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix(
                    Infix::LessThan,
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                )),
                consequence: vec![Statement::Expression(Expression::Identifier(Identifier(String::from("x"))))],
                alternative: Some(vec![Statement::Expression(Expression::Identifier(Identifier(String::from("y"))))]),
            })],
            program,
        );
    }
    
    #[test]
    fn test_func_expression() {
        let input = "fn(x, y) { x + y; }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Function {
                params: vec![Identifier(String::from("x")), Identifier(String::from("y"))],
                body: vec![Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Box::new(Expression::Identifier(Identifier(String::from("y")))),
                ))],
            })],
            program,
        );
    }

    #[test]
    fn test_func_params() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec![Identifier(String::from("x"))]),
            (
                "fn(x, y, z) {};",
                vec![
                    Identifier(String::from("x")),
                    Identifier(String::from("y")),
                    Identifier(String::from("z")),
                ],
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(
                vec![Statement::Expression(Expression::Function {
                    params: expect,
                    body: vec![],
                })],
                program,
            );
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Statement::Expression(Expression::Call {
                function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                args: vec![
                    Expression::Literal(Literal::Integer(1)),
                    Expression::Infix(
                        Infix::Multiply,
                        Box::new(Expression::Literal(Literal::Integer(2))),
                        Box::new(Expression::Literal(Literal::Integer(3))),
                    ),
                    Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Integer(4))),
                        Box::new(Expression::Literal(Literal::Integer(5))),
                    ),
                ],
            })],
            program,
        );
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
            (
                "a + add(b * c) + d",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Call {
                            function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                            args: vec![Expression::Infix(
                                Infix::Multiply,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )],
                        }),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                )),
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                Statement::Expression(Expression::Call {
                    function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    args: vec![
                        Expression::Identifier(Identifier(String::from("a"))),
                        Expression::Identifier(Identifier(String::from("b"))),
                        Expression::Literal(Literal::Integer(1)),
                        Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(2))),
                            Box::new(Expression::Literal(Literal::Integer(3))),
                        ),
                        Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Integer(4))),
                            Box::new(Expression::Literal(Literal::Integer(5))),
                        ),
                        Expression::Call {
                            function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                            args: vec![
                                Expression::Literal(Literal::Integer(6)),
                                Expression::Infix(
                                    Infix::Multiply,
                                    Box::new(Expression::Literal(Literal::Integer(7))),
                                    Box::new(Expression::Literal(Literal::Integer(8))),
                                ),
                            ],
                        },
                    ],
                }),
            ),
            (
                "add(a + b + c * d / f + g)",
                Statement::Expression(Expression::Call {
                    function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    args: vec![Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Infix(
                                Infix::Plus,
                                Box::new(Expression::Identifier(Identifier(String::from("a")))),
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                            )),
                            Box::new(Expression::Infix(
                                Infix::Divide,
                                Box::new(Expression::Infix(
                                    Infix::Multiply,
                                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                                )),
                                Box::new(Expression::Identifier(Identifier(String::from("f")))),
                            )),
                        )),
                        Box::new(Expression::Identifier(Identifier(String::from("g")))),
                    )],
                }),
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                Statement::Expression(Expression::Infix(
                    Infix::Multiply,
                    Box::new(Expression::Infix(
                        Infix::Multiply,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Index(
                            Box::new(Expression::Literal(Literal::Array(vec![
                                Expression::Literal(Literal::Integer(1)),
                                Expression::Literal(Literal::Integer(2)),
                                Expression::Literal(Literal::Integer(3)),
                                Expression::Literal(Literal::Integer(4)),
                            ]))),
                            Box::new(Expression::Infix(
                                Infix::Multiply,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                )),
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                Statement::Expression(Expression::Call {
                    function: Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    args: vec![
                        Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Identifier(Identifier(String::from("a")))),
                            Box::new(Expression::Index(
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Literal(Literal::Integer(2))),
                            )),
                        ),
                        Expression::Index(
                            Box::new(Expression::Identifier(Identifier(String::from("b")))),
                            Box::new(Expression::Literal(Literal::Integer(1))),
                        ),
                        Expression::Infix(
                            Infix::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(2))),
                            Box::new(Expression::Index(
                                Box::new(Expression::Literal(Literal::Array(vec![
                                    Expression::Literal(Literal::Integer(1)),
                                    Expression::Literal(Literal::Integer(2)),
                                ]))),
                                Box::new(Expression::Literal(Literal::Integer(1))),
                            )),
                        ),
                    ],
                }),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }
}
