use crate::settings::builtin::builtin_funcs::BuiltInFunc;
use crate::type_parser::typed_ast::{TypedAstType, TypedIdent, TypedFuncArg, TypeFlag, TypedStmt, TypedFunc, TypedExpr};
use crate::to_ts_rust::common_struct::CanAssignObj;
use once_cell::sync::Lazy;

const RUST_FUNC_DEF: &str = "
#[wasm_bindgen]
extern \"C\" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

fn console_log(value: i64) {
    console_log!(\"{}\", value);
}
";

const TS_FUNC_DEF: &str = "
const console_log = (value: number) => {
    console.log(value);
};
";

pub const CONSOLE_LOG: Lazy<BuiltInFunc> = Lazy::new(|| BuiltInFunc::new(
    "console_log",
    RUST_FUNC_DEF,
    TS_FUNC_DEF,
    TypedAstType::Func(vec![TypedAstType::Number], Box::new(TypedAstType::Void)),
    CanAssignObj::TypedFunc(TypedFunc::new(
        TypedIdent::new("console_log".to_string()),
        vec![TypedFuncArg::new(TypedIdent::new("value".to_string()), TypeFlag::NumberType)],
        vec![TypedStmt::expr_new(TypedExpr::call_expr_new(
            TypedAstType::Void,
            TypedIdent::new("console_log".to_string()),
            vec![TypedExpr::num_ident_new(TypedIdent::new("value".to_string()))]
        ))],
        TypedAstType::Void
    ))));
