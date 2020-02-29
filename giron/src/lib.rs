mod codepoint;
mod errors;
mod estree;
mod parser;
mod scanner;
mod span;
mod token;

use crate::errors::errors::{Error, ErrorType};
use crate::errors::result::Result;
use crate::estree::estree::Node;
use crate::parser::parser::Parser;

/// GironError
pub type GironError = Error;

/// GironErrorType
pub type GironErrorType = ErrorType;

/// EstreeNode
pub type EstreeNode = Node;

/// Parser a source string as an ECMAScript module.
pub fn parse_module(source: String) -> Result<Node> {
    let mut parser = Parser::new(source.chars());
    parser.parse_module()
}

/// Parser a source string as an ECMAScript script.
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
