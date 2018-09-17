use ast::*;
use object::Object;

#[derive(Debug)]
pub struct Evaluator {
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator{}
    }
    
    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::Null | Object::Bool(false) => false,
            _ => true,
        }
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
    
    pub fn eval_block_statement(&mut self, block: BlockStatement) -> Option<Object> {
        let mut result = None;

        for statement in block {
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
            Expression::Infix(infix, left, right) => {
                let left = self.eval_expression(*left);
                let right = self.eval_expression(*right);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expression(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            },
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(*condition, consequence, alternative),
            _ => None, // TODO
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

    fn eval_infix_expression(&mut self, infix: Infix, left: Object, right: Object) -> Object {
        match infix {
            Infix::Plus
            | Infix::Minus
            | Infix::Multiply
            | Infix::Divide
            | Infix::LessThan
            | Infix::GreaterThan => {
                match left {
                    Object::Integer(left) => {
                        match right {
                            Object::Integer(right) => {
                                self.eval_infix_integer_expression(infix, left, right)
                            },
                            _ => Object::Error(format!("type mismatch: {} {} {}", left, infix, right)),
                        }
                    },
                    _ => Object::Error(format!("unknown operator: {} {} {}", left, infix, right)),
                }
            },
            Infix::Equal    => Object::Bool(left == right),
            Infix::NotEqual => Object::Bool(left != right),
        }

    }

    fn eval_infix_integer_expression(&mut self, infix: Infix, left: i64, right: i64) -> Object {
        match infix {
            Infix::Plus        => Object::Integer(left + right),
            Infix::Minus       => Object::Integer(left - right),
            Infix::Multiply    => Object::Integer(left * right),
            Infix::Divide      => Object::Integer(left / right),
            Infix::Equal       => Object::Bool(left == right),
            Infix::NotEqual    => Object::Bool(left != right),
            Infix::LessThan    => Object::Bool(left < right),
            Infix::GreaterThan => Object::Bool(left > right),
        }
    }
    
    fn eval_if_expression(&mut self, condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Option<Object> {
        let condition = match self.eval_expression(condition) {
            Some(condition) => condition,
            None => return None,
        };

        if Self::is_truthy(condition) {
            self.eval_block_statement(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_block_statement(alternative)
        } else {
            None
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
            ("5", Some(Object::Integer(5))),
            ("10", Some(Object::Integer(10))),
            ("-5", Some(Object::Integer(-5))),
            ("-10", Some(Object::Integer(-10))),
            ("5 + 5 + 5 + 5 - 10", Some(Object::Integer(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::Integer(32))),
            ("-50 + 100 + -50", Some(Object::Integer(0))),
            ("5 * 2 + 10", Some(Object::Integer(20))),
            ("5 + 2 * 10", Some(Object::Integer(25))),
            ("20 + 2 * -10", Some(Object::Integer(0))),
            ("50 / 2 * 2 + 10", Some(Object::Integer(60))),
            ("2 * (5 + 10)", Some(Object::Integer(30))),
            ("3 * 3 * 3 + 10", Some(Object::Integer(37))),
            ("3 * (3 * 3) + 10", Some(Object::Integer(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::Integer(50))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
            ("1 < 2", Some(Object::Bool(true))),
            ("1 > 2", Some(Object::Bool(false))),
            ("1 < 1", Some(Object::Bool(false))),
            ("1 > 1", Some(Object::Bool(false))),
            ("1 == 1", Some(Object::Bool(true))),
            ("1 != 1", Some(Object::Bool(false))),
            ("1 == 2", Some(Object::Bool(false))),
            ("1 != 2", Some(Object::Bool(true))),
            ("true == true", Some(Object::Bool(true))),
            ("false == false", Some(Object::Bool(true))),
            ("true == false", Some(Object::Bool(false))),
            ("true != false", Some(Object::Bool(true))),
            ("false != true", Some(Object::Bool(true))),
            ("(1 < 2) == true", Some(Object::Bool(true))),
            ("(1 < 2) == false", Some(Object::Bool(false))),
            ("(1 > 2) == true", Some(Object::Bool(false))),
            ("(1 > 2) == false", Some(Object::Bool(true))),
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

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Some(Object::Integer(10))),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(Object::Integer(10))),
            ("if (1 < 2) { 10 }", Some(Object::Integer(10))),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(Object::Integer(20))),
            ("if (1 < 2) { 10 } else { 20 }", Some(Object::Integer(10))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
