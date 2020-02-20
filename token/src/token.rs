use crate::value::Value;
use std::char;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    BooleanLiteral,
    Identifier,
    Keyword,
    NullLiteral,
    NumericLiteral,
    StringLiteral,
    Punctuator,
    RegularExpression,
    Template,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tokentype: TokenType,
    pub value: Value,
    pub pattern: Option<String>,
    pub flags: Option<Vec<char>>,
    pub octal: Option<bool>,
    pub cooked: Option<String>,
    pub head: Option<bool>,
    pub tail: Option<bool>,
    pub line_num: usize,
    pub col: usize,
    pub start: usize,
    pub end: usize,
}
