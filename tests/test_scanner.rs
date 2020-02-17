use scanner::scanner::{LexGoal, Scanner};
use std::fs;
use token::token::TokenType;

#[test]
fn test_scanner() {
    let mut scnr = Scanner::new("+(a)".chars());
    loop {
        let token = scnr.next(LexGoal::RegExp).unwrap();
        match token.tokentype {
            TokenType::Eof => break,
            _ => {
                println!("{:?} {:?}", token.tokentype, token.value);
            }
        }
    }
}

#[test]
fn test_snippet() {
    let source = fs::read_to_string("tests/snippets/unfold.txt").unwrap();
    let mut scnr = Scanner::new(source.chars());
    loop {
        let token = scnr.next(LexGoal::RegExp).unwrap();
        match token.tokentype {
            TokenType::Eof => break,
            _ => {
                println!("{:?} {:?}", token.tokentype, token.value);
            }
        }
    }
}
