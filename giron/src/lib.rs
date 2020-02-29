pub mod codepoint;
pub mod errors;
pub mod estree;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod token;

use crate::errors::result::Result;
use crate::estree::estree::Node;
use crate::parser::parser::Parser;

pub fn parse_module(source: String) -> Result<Node> {
    let mut parser = Parser::new(source.chars());
    parser.parse_module()
}

pub fn parse_script(source: String) -> Result<Node> {
    let mut parser = Parser::new(source.chars());
    parser.parse_script()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
