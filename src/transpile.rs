use crate::parser::parser::parser;
use crate::type_parser::type_parser::type_parser;
use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::settings::builtin::builtin_funcs::BUILTIN_FUNCS;

pub fn transpile_to_ts(reglico_code: &str) -> String {
    let stmts = parser(reglico_code);

    let typed_stmts = type_parser(stmts);

    let mut ts_code = "".to_string();

    for func in BUILTIN_FUNCS {
        ts_code = format!("{}{}", ts_code, func.get_ts_func_def());
    }

    ts_code = format!("{}{}", ts_code, ToTs::to_ts(typed_stmts, None));

    ts_code

}

pub fn transpile_to_rust(reglico_code: &str) -> String {
    let stmts = parser(reglico_code);

    let typed_stmts = type_parser(stmts);

    let mut rust_code = "".to_string();

    for func in BUILTIN_FUNCS {
        rust_code = format!("{}{}", rust_code, func.get_rust_func_def());
    }

    rust_code = format!("{}{}", rust_code, ToRust::to_rust(typed_stmts, None));
    rust_code

}
