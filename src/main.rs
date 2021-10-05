mod parser;
mod to_ts_rust;
mod type_parser;
mod transpile;

use std::env;
use std::fs;
use std::io::Write;
use crate::transpile::{transpile_to_ts, transpile_to_rust};

fn main() {

    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let reglico_code = fs::read_to_string(filename).expect("can't read specified file ");

    let ts_code = transpile_to_ts(reglico_code.as_str());

    let rust_code = transpile_to_rust(reglico_code.as_str());

    fs::create_dir_all("output").expect("can't create output directory");
    let mut ts_file = fs::File::create("output/output.ts").expect("can't create output/output.ts");
    let mut rust_file = fs::File::create("output/output.rs").expect("can't create output/output.rs");

    ts_file.write_all(ts_code.as_bytes());
    rust_file.write_all(rust_code.as_bytes());

}
