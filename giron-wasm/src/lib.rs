mod utils;

use parser::parser::Parser;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(js_name = parseScript)]
pub fn parse_script(source: String) -> JsValue {
    let mut parser = Parser::new(source.chars());
    let ast = parser.parse_script().unwrap();
    JsValue::from_serde(&ast).unwrap()
}

#[wasm_bindgen(js_name = parseModule)]
pub fn parse_module(source: String) -> JsValue {
    let mut parser = Parser::new(source.chars());
    let ast = parser.parse_module().unwrap();
    JsValue::from_serde(&ast).unwrap()
}
