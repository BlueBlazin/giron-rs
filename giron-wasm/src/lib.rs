mod utils;

use giron;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(js_name = parseScript)]
pub fn parse_script(source: String) -> JsValue {
    match giron::parse_script(source) {
        Ok(ast) => JsValue::from_serde(&ast).unwrap(),
        Err(err) => JsValue::from(format!("{:?}", err)),
    }
}

#[wasm_bindgen(js_name = parseModule)]
pub fn parse_module(source: String) -> JsValue {
    match giron::parse_module(source) {
        Ok(ast) => JsValue::from_serde(&ast).unwrap(),
        Err(err) => JsValue::from(format!("{:?}", err)),
    }
}
