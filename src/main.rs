mod ast;
mod emitter;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::fs::read_to_string;

fn main() {
    let name = "average";
    let source = read_to_string(format!("examples/{}.txt", name)).unwrap();
    let out_path = format!("examples/output/{}.c", name);

    let mut lexer = Lexer::new(source);
    let mut emitter = emitter::Emitter::new(out_path.to_string());

    if let Err(e) = lexer.validate() {
        eprintln!("Encountered an error lexing the source code. \n\n {}", e);
        return;
    }
    while let Ok(token) = lexer.get_token() {
        if matches!(token.kind, lexer::TokenKind::Newline) {
            println!();
            continue;
        }
        print!("{} ", token);
        if token.kind == lexer::TokenKind::EOF {
            break;
        }
    }

    let mut parser = Parser::new(lexer, &mut emitter);
    match parser.program() {
        Ok(p) => println!("Parsed program: {:#?}", p),
        Err(e) => {
            eprintln!("Encountered an error parsing the source code. \n\n {}", e);
            return;
        }
    }

    emitter.print();
    emitter.write().expect("Failed to write to file.");
}
