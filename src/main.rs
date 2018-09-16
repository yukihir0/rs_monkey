mod token;
mod ast;
mod object;
mod lexer;
mod parser;
mod evaluator;

use std::io::{self, BufRead, Write};
use lexer::Lexer;
use parser::Parser;
use evaluator::Evaluator;

fn main() {
    let stdin = io::stdin();

    loop {
        print!(">> ");
        io::stdout().flush().expect("Error flushing stdout");

        let mut evaluator = Evaluator::new();

        let mut line = String::new();
        stdin.lock().read_line(&mut line).expect("Error reading from stdin");
        let mut parser = Parser::new(Lexer::new(&mut line));

        let program = parser.parse();
        let errors = parser.get_errors();
        
        if errors.len() > 0 {
            for err in errors {
                println!("{}", err);
            }
            continue;
        }

        if let Some(evaluated) = evaluator.eval(program) {
            println!("{}\n", evaluated);
        }
    }
}
