use parser::parser::Parser;
use std::env;
use std::fs;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    let source = fs::read_to_string(args.pop().unwrap()).unwrap();
    let mut parser = Parser::new(source.chars());
    match parser.parse_module() {
        Ok(ast) => println!("{}", serde_json::to_string_pretty(&ast).unwrap()),
        Err(e) => println!("{}\n{:?}", &e, e),
    }
}
