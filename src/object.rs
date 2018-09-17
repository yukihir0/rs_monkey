use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    Null,
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(ref value) => write!(f, "{}", value),
            Object::Bool(ref value)    => write!(f, "{}", value),
            Object::Null               => write!(f, "null"),
            Object::Error(ref value)   => write!(f, "{}", value),
        }
    }
}


