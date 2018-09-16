use ast::*;
use object::Object;

#[derive(Debug)]
pub struct Evaluator {
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator{}
    }
    
    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program {
            match self.eval_statement(statement) {
                obj => result = obj,
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            _                                 => None,
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            _                            => None, // TODO
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(value),
            Literal::Bool(value)    => Object::Bool(value),
        }
    }
}

#[cfg(test)]
mod tests {
    use evaluator::*;
    use lexer::Lexer;
    use parser::Parser;

    fn eval(input: &str) -> Option<Object> {
        Evaluator::new().eval(Parser::new(Lexer::new(input)).parse())
    }

    #[test]
    fn test_integer_expression() {
        let tests = vec![
            ("5",  Some(Object::Integer(5))),
            ("10", Some(Object::Integer(10))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            ("true",  Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
