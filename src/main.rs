mod parser;
mod to_ts_rust;
mod type_parser;
mod transpile;

use std::env;
use std::fs;
use std::io::Write;
use crate::transpile::{transpile_to_ts, transpile_to_rust};

fn create_wasm_setting_rust_code(rust_code: String) -> String {
    let pre_rust_code = "use wasm_bindgen::prelude::*;use web_sys::console;#[cfg(feature = \"wee_alloc\")]#[global_allocator]
    static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    #[wasm_bindgen(start)]
    pub fn main_js() -> Result<(), JsValue> {
        #[cfg(debug_assertions)]
        console_error_panic_hook::set_once();
    ";

    let after_rust_code = "
    Ok(())
    }";

    format!("{}{}{}", pre_rust_code, rust_code, after_rust_code)

}

fn main() {

    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let reglico_code = fs::read_to_string(filename).expect("can't read specified file ");

    let ts_code = transpile_to_ts(reglico_code.as_str());

    let rust_code = create_wasm_setting_rust_code(transpile_to_rust(reglico_code.as_str()));

    let mut ts_file = fs::File::create("output/ts_output/output.ts").expect("can't create output/output.ts");
    let mut rust_file = fs::File::create("output/src/lib.rs").expect("can't create output/output.rs");

    ts_file.write_all(ts_code.as_bytes());
    rust_file.write_all(rust_code.as_bytes());

}
