use giron::{parse_module, parse_script};

macro_rules! assert_fails {
    ( $source:expr ) => {{
        if let Ok(_) = parse_module(String::from($source)) {
            panic!("Expected parse failure.")
        }
    }};
    ( $source:expr , script ) => {{
        if let Ok(_) = parse_script(String::from($source)) {
            panic!("Expected parse failure.")
        }
    }};
}

// 12 Expressions

// 12.1 Identifiers
#[test]
fn test_identifiers() {
    assert_fails!("function foo() { 'use strict'; function bar({ arguments }) {} }");
    assert_fails!("function foo() { 'use strict'; function bar({ eval }) {} }");
    assert_fails!("function foo() { 'use strict'; let yield; }");
    assert_fails!("let await;");
    assert_fails!("({ await }) => {}");
}
