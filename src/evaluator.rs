use ast::*;
use object::Object;
use environment::Environment;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(environment: Rc<RefCell<Environment>>) -> Self {
        Evaluator {
            environment: environment,
        }
    }
    
    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::Null | Object::Bool(false) => false,
            _ => true,
        }
    }

    fn error(msg: String) -> Object {
        Object::Error(msg)
    }

    fn is_error(obj: &Object) -> bool {
        match obj {
            Object::Error(_) => true,
            _ => false,
        }
    }

    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(*value),
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                obj => result = obj,
            }
        }

        result
    }
    
    pub fn eval_block_statement(&mut self, block: BlockStatement) -> Option<Object> {
        let mut result = None;

        for statement in block {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                obj => result = obj,
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Let(identifier, expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };
                if Self::is_error(&value) {
                    Some(value)
                } else {
                    let Identifier(name) = identifier;
                    self.environment.borrow_mut().set(name, &value);
                    None
                }
            },
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };
                if Self::is_error(&value) {
                    Some(value)
                } else {
                    Some(Object::ReturnValue(Box::new(value)))
                }
            },
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Identifier(identifier) => Some(self.eval_identifier(identifier)),
            Expression::Literal(literal)       => Some(self.eval_literal(literal)),
            Expression::Prefix(prefix, right)  => {
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
            Expression::Function { params, body } => Some(Object::Function(params, body, Rc::clone(&self.environment))),
            Expression::Call { function, args } => Some(self.eval_call_expression(function, args)),
            Expression::Index(left, index) => {
                let left = self.eval_expression(*left);
                let index = self.eval_expression(*index);
                if left.is_some() && index.is_some() {
                    Some(self.eval_index_expression(left.unwrap(), index.unwrap()))
                } else {
                    None
                }
            }
        }
    }

    fn eval_identifier(&mut self, identifier: Identifier) -> Object {
        let Identifier(name) = identifier;

        match self.environment.borrow_mut().get(name.clone()) {
            Some(value) => value,
            None => Object::Error(String::from(format!("identifier not found: {}", name))),
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(value),
            Literal::Bool(value)    => Object::Bool(value),
            Literal::String(value)  => Object::String(value),
            Literal::Array(objects) => self.eval_array_literal(objects),
        }
    }

    fn eval_array_literal(&mut self, objects: Vec<Expression>) -> Object {
        Object::Array(
            objects
                .iter()
                .map(|e| self.eval_expression(e.clone()).unwrap_or(Object::Null))
                .collect::<Vec<_>>(),
        )
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
            _ => Self::error(format!("unknown operator: -{}", right)),
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
                            _ => Self::error(format!("type mismatch: {} {} {}", left, infix, right)),
                        }
                    },
                    Object::String(left) => {
                        match right {
                            Object::String(right) => {
                                self.eval_infix_string_expression(infix, left, right)
                            },
                            _ => Self::error(format!("type mismatch: {} {} {}", left, infix, right)),
                        }
                    },
                    _ => Self::error(format!("unknown operator: {} {} {}", left, infix, right)),
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

    fn eval_infix_string_expression(&mut self, infix: Infix, left: String, right: String) -> Object {
        match infix {
            Infix::Plus => Object::String(format!("{}{}", left, right)),
            _ => Object::Error(String::from(format!(
                "unknown operator: {} {} {}",
                left, infix, right
            ))),
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

    fn eval_call_expression(&mut self, function: Box<Expression>, args: Vec<Expression>) -> Object {
        // 引数
        let args = args
            .iter()
            .map(|e| self.eval_expression(e.clone()).unwrap_or(Object::Null))
            .collect::<Vec<_>>();

        // 関数
        let (params, body, environment) = match self.eval_expression(*function) {
            Some(Object::Function(params, body, environment)) => (params, body, environment),
            Some(Object::Builtin(function)) => return function(args),
            Some(o) => return Self::error(format!("{} is not valid function", o)),
            None => return Object::Null,
        };
        
        // 関数の仮引数と引数の数をチェックする
        if params.len() != args.len() {
            return Self::error(format!(
                "wrong number of arguments: {} expected but {} given",
                params.len(),
                args.len()
            ));
        }

        // 現在の環境を退避する
        let current_environment = Rc::clone(&self.environment);

        // 関数呼出の環境を作成する
        let mut function_environment = Environment::new_with_outer(Rc::clone(&environment));

        // 関数呼出の環境に引数をセットする
        let list = params.iter().zip(args.iter());
        for (_, (identifier, value)) in list.enumerate() {
            let Identifier(name) = identifier.clone();
            function_environment.set(name, value);
        }

        // 関数呼出の環境で式を評価する
        self.environment = Rc::new(RefCell::new(function_environment));
        let result = self.eval_block_statement(body);

        // 現在の環境を復元する
        self.environment = current_environment;

        match result {
            Some(object) => object,
            None => Object::Null,
        }
    }

    fn eval_index_expression(&mut self, left: Object, index: Object) -> Object {
        match left {
            Object::Array(ref array) => if let Object::Integer(index) = index {
                self.eval_array_index_expression(array.clone(), index)
            } else {
                Self::error(format!("index operator not supported: {}", left))
            },
            _ => Self::error(format!("uknown operator: {} {}", left, index)),
        }
    }

    fn eval_array_index_expression(&mut self, array: Vec<Object>, index: i64) -> Object {
        let max = array.len() as i64;

        if index < 0 || index > max {
            return Object::Null;
        }

        match array.get(index as usize) {
            Some(o) => o.clone(),
            None => Object::Null,
        }
    }
}

#[cfg(test)]
mod tests {
    use builtin;
    use lexer::Lexer;
    use parser::Parser;
    use evaluator::*;

    fn eval(input: &str) -> Option<Object> {
        Evaluator::new(Rc::new(RefCell::new(Environment::from(builtin::new()))))
            .eval(Parser::new(Lexer::new(input)).parse())
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
    fn test_string_expression() {
        let input = "\"Hello World!\"";

        assert_eq!(
            Some(Object::String(String::from("Hello World!"))),
            eval(input)
        );
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        assert_eq!(
            Some(Object::String(String::from("Hello World!"))),
            eval(input)
        );
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

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Some(Object::Integer(10))),
            ("return 10; 9;", Some(Object::Integer(10))),
            ("return 2 * 5; 9;", Some(Object::Integer(10))),
            ("9; return 2 * 5; 9;", Some(Object::Integer(10))),
            (
                r#"
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  return 1;
}"#,
                Some(Object::Integer(10)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "5 + true; 5;",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "-true",
                Some(Object::Error(String::from("unknown operator: -true"))),
            ),
            (
                "5; true + false; 5;",
                Some(Object::Error(String::from("unknown operator: true + false"))),
            ),
            (
                "if (10 > 1) { true + false; }",
                Some(Object::Error(String::from("unknown operator: true + false"))),
            ),
            (
                r#"
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }
  return 1;
}"#,
                Some(Object::Error(String::from("unknown operator: true + false"))),
            ),
            (
                "foobar",
                Some(Object::Error(String::from("identifier not found: foobar"))),
            ),
            (
                "\"Hello\" - \"World\"",
                Some(Object::Error(String::from("unknown operator: Hello - World"))),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Some(Object::Integer(5))),
            ("let a = 5 * 5; a;", Some(Object::Integer(25))),
            ("let a = 5; let b = a; b;", Some(Object::Integer(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Some(Object::Integer(15)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        assert_eq!(
            Some(Object::Function(
                vec![Identifier(String::from("x"))],
                vec![Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                ))],
                Rc::new(RefCell::new(Environment::from(builtin::new()))),
            )),
            eval(input),
        );
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Some(Object::Integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Some(Object::Integer(5)),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Some(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Some(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Some(Object::Integer(20)),
            ),
            (
                "fn(x) { x; }(5)",
                Some(Object::Integer(5))
            ),
            (
                "fn(a) { let f = fn(b) { a + b }; f(a); }(5);",
                Some(Object::Integer(10)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
let newAdder = fn(x) {
  fn(y) { x + y };
}
let addTwo = newAdder(2);
addTwo(2);
        "#;

        assert_eq!(Some(Object::Integer(4)), eval(input));
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            // len
            (
                "len(\"\")",
                Some(Object::Integer(0))
            ),
            (
                "len(\"four\")",
                Some(Object::Integer(4))
            ),
            (
                "len(\"hello world\")",
                Some(Object::Integer(11))
            ),
            (
                "len(1)",
                Some(Object::Error(String::from("argument to `len` not supported, got 1"))),
            ),
            (
                "len(\"one\", \"two\")",
                Some(Object::Error(String::from("wrong number of arguments. got=2, want=1"))),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        assert_eq!(
            Some(Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ])),
            eval(input),
        );
    }

    #[test]
    fn test_array_index_expression() {
        let tests = vec![
            (
                "[1, 2, 3][0]",
                Some(Object::Integer(1))
            ),
            (
                "[1, 2, 3][1]",
                Some(Object::Integer(2))
            ),
            (
                "let i = 0; [1][i]",
                Some(Object::Integer(1))
            ),
            (
                "[1, 2, 3][1 + 1];",
                Some(Object::Integer(3))
            ),
            (
                "let myArray = [1, 2, 3]; myArray[2];",
                Some(Object::Integer(3))
            ),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(Object::Integer(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Some(Object::Integer(2)),
            ),
            (
                "[1, 2, 3][3]",
                Some(Object::Null)
            ),
            (
                "[1, 2, 3][-1]",
                Some(Object::Null)
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
