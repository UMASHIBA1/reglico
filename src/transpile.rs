use crate::parser::parser::parser;
use crate::type_parser::type_parser::type_parser;
use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::to_ts_rust::to_rust::to_rust::ToRust;

pub fn transpile_to_ts(reglico_code: &str) -> String {
    let stmts = parser(reglico_code);

    let typed_stmts = type_parser(stmts);

    ToTs::to_ts(typed_stmts, None)

}

pub fn transpile_to_rust(reglico_code: &str) -> String {
    let stmts = parser(reglico_code);

    let typed_stmts = type_parser(stmts);

    ToRust::to_rust(typed_stmts, None)

}
