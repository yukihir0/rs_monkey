use object::*;
use std::collections::HashMap;

pub fn new() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(monkey_len));
    builtins
}

fn monkey_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            1,
        ));
    }

    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}
