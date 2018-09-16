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
            Expression::Literal(literal)      => Some(self.eval_literal(literal)),
            Expression::Prefix(prefix, right) => {
                if let Some(right) = self.eval_expression(*right) {
                    Some(self.eval_prefix_expression(prefix, right))
                } else {
                    None
                }
            },
            _                                 => None, // TODO
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(value),
            Literal::Bool(value)    => Object::Bool(value),
        }
    }

    fn eval_prefix_expression(&mut self, prefix: Prefix, right: Object) -> Object {
        match prefix {
            Prefix::Not   => self.eval_bang_operator_expression(right),
            Prefix::Plus  => Object::Null, // TODO
            Prefix::Minus => self.eval_minus_prefix_operator_expression(right),
        }
    }

    fn eval_bang_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Bool(true)  => Object::Bool(false),
            Object::Bool(false) => Object::Bool(true),
            Object::Null        => Object::Bool(true),
            _                   => Object::Bool(false),
        }
    }

    fn eval_minus_prefix_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Integer(value) => Object::Integer(-value),
            _                      => Object::Null,
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
            ("5",   Some(Object::Integer(5))),
            ("10",  Some(Object::Integer(10))),
            ("-5",  Some(Object::Integer(-5))),
            ("-10", Some(Object::Integer(-10))),
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

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true",   Some(Object::Bool(false))),
            ("!false",  Some(Object::Bool(true))),
            ("!!true",  Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!5",     Some(Object::Bool(true))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
