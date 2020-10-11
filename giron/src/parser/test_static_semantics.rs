use crate::errors::result::Result;
use crate::parser::parser::Parser;
use std::str::Chars;

fn assert_fails<F, T>(source: &str, f: F)
where
    F: Fn(Parser<Chars>) -> Result<T>,
{
    let parser = Parser::new(source.chars());
    if let Ok(_) = f(parser) {
        panic!("Expected parse failure.");
    }
}

// 12 Expressions

// 12.1 Identifiers
#[test]
fn test_identifiers() {
    assert_fails("arguments", |mut p| {
        p.ctx.strict = true;
        p.parse_binding_id()
    });
    assert_fails("eval", |mut p| {
        p.ctx.strict = true;
        p.parse_binding_id()
    });
    assert_fails("yield", |mut p| {
        p.ctx.strict = true;
        p.parse_id_reference()
    });
    assert_fails("await", |mut p| {
        p.ctx.strict = true;
        p.parse_id_reference()
    });
}
