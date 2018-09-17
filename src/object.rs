use ast::*;
use environment::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    String(String),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(fn(Vec<Object>) -> Object),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(ref value)         => write!(f, "{}", value),
            Object::Bool(ref value)            => write!(f, "{}", value),
            Object::String(ref value)          => write!(f, "{}", value),
            Object::Function(ref params, _, _) => {
                let mut result = String::new();
                for (i, Identifier(ref s)) in params.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}", s));
                    } else {
                        result.push_str(&format!(", {}", s));
                    }
                }
                write!(f, "fn({}) {{ ... }}", result)
            },
            Object::Builtin(_)             => write!(f, "[builtin function]"),
            Object::Null                   => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{}", value),
            Object::Error(ref value)       => write!(f, "{}", value),
        }
    }
}


