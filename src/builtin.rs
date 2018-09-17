use object::*;
use std::collections::HashMap;

pub fn new() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(monkey_len));
    builtins.insert(String::from("first"), Object::Builtin(monkey_first));
    builtins.insert(String::from("last"), Object::Builtin(monkey_last));
    builtins.insert(String::from("rest"), Object::Builtin(monkey_rest));
    builtins.insert(String::from("push"), Object::Builtin(monkey_push));
    builtins.insert(String::from("puts"), Object::Builtin(monkey_puts));
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
        Object::Array(o)  => Object::Integer(o.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}

fn monkey_first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            1,
        ));
    }

    match &args[0] {
        Object::Array(o) => if let Some(ao) = o.first() {
            ao.clone()
        } else {
            Object::Null
        },
        o => Object::Error(format!("argument to `first` must be array. got {}", o)),
    }
}

fn monkey_last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            1,
        ));
    }

    match &args[0] {
        Object::Array(o) => if let Some(ao) = o.last() {
            ao.clone()
        } else {
            Object::Null
        },
        o => Object::Error(format!("argument to `last` must be array. got {}", o)),
    }
}

fn monkey_rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            1,
        ));
    }

    match &args[0] {
        Object::Array(o) => if o.len() > 0 {
            Object::Array(o[1..].to_vec())
        } else {
            Object::Null
        },
        o => Object::Error(format!("argument to `rest` must be array. got {}", o)),
    }
}

fn monkey_push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            2,
        ));
    }

    match &args[0] {
        Object::Array(o) => {
            let mut arr = o.clone();
            arr.push(args[1].clone());
            Object::Array(arr)
        }
        o => Object::Error(format!("argument to `push` must be array. got {}", o)),
    }
}

fn monkey_puts(args: Vec<Object>) -> Object {
    for arg in &args {
        println!("{}", arg);
    }

    Object::Null
}
