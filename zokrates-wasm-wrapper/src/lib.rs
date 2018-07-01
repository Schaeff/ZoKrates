#![feature(proc_macro, wasm_custom_section, wasm_import_module)]

extern crate wasm_bindgen;
extern crate zokrates;

use std::io::BufReader;
use wasm_bindgen::prelude::*;
use zokrates::compile::compile;
use zokrates::field::FieldPrime;

#[wasm_bindgen]
extern {
	type HTMLDocument;
    static document: HTMLDocument;

    type Element;
    #[wasm_bindgen(method, setter = innerHTML)]
    fn set_inner_html(this: &Element, html: &str);
    #[wasm_bindgen(method)]
    fn getElementById(this: &HTMLDocument, tagName: &str) -> Element;
}

#[wasm_bindgen]
pub fn exec(source: &str) {
	let mut source = BufReader::new(source.as_bytes());
	let flattened = compile::<FieldPrime, _>(&mut source, false, false);
	let out = document.getElementById("out");

	let res = match flattened {
		Err(e) => format!("Error: {}", e),
		Ok(r) => format!("{}", r)
	};

    out.set_inner_html(&format!("{}", res));
}