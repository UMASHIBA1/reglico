mod parser;
mod to_ts_rust;
mod type_parser;

fn main() {

    let stmts = parser::parser::parser("1;");

    let typed_stmts = type_parser::type_parser::type_parser(stmts);

    let ts_code = to_ts_rust::to_ts::to_ts::ToTs::to_ts(typed_stmts.clone(), None);

    let rust_code = to_ts_rust::to_rust::to_rust::ToRust::to_rust(typed_stmts, None);

    println!("ts code: {}, rust code: {}", ts_code, rust_code);

}
