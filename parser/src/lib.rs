pub mod parser;
pub mod syntax;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_binary_expr() {
        let mut parser = parser::Parser::new("1 ** 4 + 2 * 3".chars());
        match parser.parse_binary_expr() {
            Ok(ast) => {
                let serialized = serde_json::to_string_pretty(&ast).unwrap();
                println!("{}", serialized);
            }
            Err(error) => println!("{:?}", error),
        }
    }

    #[test]
    fn test_parse_primary_expr_this() {
        let mut parser = parser::Parser::new("this".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_id() {
        let mut parser = parser::Parser::new("foo".chars());
        parser.parse_primary_expr().unwrap();
        let mut parser = parser::Parser::new("yield".chars());
        parser.parse_primary_expr().unwrap();
        let mut parser = parser::Parser::new("await".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_lit() {
        let mut parser = parser::Parser::new("0.1732E+27".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_array() {
        let mut parser = parser::Parser::new("[0, 1, ...x, 3, ...y]".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_object() {
        let mut parser = parser::Parser::new("{ x: 0, y: { a, ...b }, [z]: [] }".chars());
        parser.parse_primary_expr().unwrap();
        let mut parser = parser::Parser::new("{ super: 0, new: 1, function: 2 }".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_regexp() {
        let mut parser = parser::Parser::new("/[a-zA-Z]+/g".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_template() {
        let mut parser = parser::Parser::new("`hello, ${name}, you are ${age} years old`".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_primary_expr_arrow() {
        let mut parser = parser::Parser::new("(x, { y, ...a }, z) => 0".chars());
        parser.parse_primary_expr().unwrap();
    }

    #[test]
    fn test_parse_expression_assignment_destructuring() {
        let mut parser = parser::Parser::new("{...foo.bar} = {}".chars());
        parser.parse_expression().unwrap();
    }

    #[test]
    fn test_sandbox() {
        let mut parser = parser::Parser::new("({ get: function () { return; }})".chars());
        // let ast = parser.parse_module().unwrap();
        // let serialized = serde_json::to_string_pretty(&ast).unwrap();
        match parser.parse_module() {
            Ok(ast) => println!("{}", serde_json::to_string_pretty(&ast).unwrap()),
            Err(e) => println!("{}\n{:?}", &e, e),
        }
        // println!("{}", serialized);
    }
}
