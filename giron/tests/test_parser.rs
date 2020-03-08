use giron::{parse_module, parse_script};
use glob::glob;
use std::fs;

#[test]
fn test_parser() {
    // let mut parser = Parser::new("[1, 2, 4]".chars());
    // parser.parse_script().unwrap();
    parse_script(String::from("let x = 0;")).unwrap();
}

#[test]
fn test_snippet() {
    let source = fs::read_to_string("tests/snippets/all.txt").unwrap();
    match parse_module(source) {
        Ok(_) => (),
        Err(e) => println!("{}\n{:?}", &e, e),
    }
}

#[test]
fn test_all_snippets() {
    // test on js code snippets from 30-seconds-of-code
    // https://github.com/30-seconds/30-seconds-of-code
    glob("tests/snippets/*.txt").unwrap().for_each(|entry| {
        if let Ok(path) = entry {
            let source = fs::read_to_string(path.clone()).unwrap();
            // let mut parser = Parser::new(source.chars());
            // match parser.parse_module() {
            //     Ok(_) => (),
            //     Err(e) => println!("{}, {}\n{:?}", path.display(), &e, e),
            // }
            // parser.parse_module().unwrap();
            parse_module(source).unwrap();
        }
    });
}

#[test]
fn test_react() {
    let source = fs::read_to_string("tests/source/react.js").unwrap();
    // let mut parser = Parser::new(source.chars());
    // parser.parse_module().unwrap();
    parse_module(source).unwrap();
}

#[test]
fn test_react_dom() {
    let source = fs::read_to_string("tests/source/react-dom.js").unwrap();
    // let mut parser = Parser::new(source.chars());
    // parser.parse_module().unwrap();
    parse_module(source).unwrap();
}

#[test]
fn test_angular() {
    let source = fs::read_to_string("tests/source/angular.js").unwrap();
    // let mut parser = Parser::new(source.chars());
    // parser.parse_module().unwrap();
    parse_module(source).unwrap();
}
