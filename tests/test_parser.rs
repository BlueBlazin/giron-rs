use glob::glob;
use parser::parser::Parser;
use std::fs;

#[test]
fn test_parser() {
    let mut parser = Parser::new("for (const [i, val] of x) x;".chars());
    parser.parse_script().unwrap();
}

#[test]
fn test_snippet() {
    let source = fs::read_to_string("tests/snippets/all.txt").unwrap();
    let mut parser = Parser::new(source.chars());
    match parser.parse_module() {
        Ok(_) => (),
        Err(e) => println!("{}\n{:?}", &e, e),
    }
}

#[test]
fn test_all_snippets() {
    glob("tests/snippets/*.txt").unwrap().for_each(|entry| {
        if let Ok(path) = entry {
            let source = fs::read_to_string(path.clone()).unwrap();
            let mut parser = Parser::new(source.chars());
            match parser.parse_module() {
                Ok(_) => (),
                Err(e) => println!("{}, {}\n{:?}", path.display(), &e, e),
            }
        }
    });
}
