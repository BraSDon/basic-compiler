mod ast;
mod emitter;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs::read_to_string;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let source = read_to_string(format!("examples/{}.txt", args[1])).expect("Failed to read file.");
    let out_path = format!("examples/c-files/{}.c", args[1]);

    let mut lexer = Lexer::new(source);
    let mut emitter = emitter::Emitter::new(out_path.to_string());

    if let Err(e) = lexer.validate() {
        eprintln!("Encountered an error lexing the source code. \n\n {}", e);
        return;
    }
    let mut parser = Parser::new(lexer, &mut emitter);
    if let Err(e) = parser.program() {
        eprintln!("Encountered an error parsing the source code. \n\n {}", e);
        return;
    }

    emitter.write().expect("Failed to write file.");
}
