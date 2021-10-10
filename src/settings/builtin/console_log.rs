use crate::settings::builtin::builtin_funcs::BuiltInFunc;
use crate::type_parser::typed_ast::{TypedAstType, TypedIdent, TypedFuncArg, TypeFlag, TypedStmt, TypedFunc, TypedExpr};
use crate::to_ts_rust::common_struct::CanAssignObj;
use once_cell::sync::Lazy;

const rust_func_def: &str = "
#[wasm_bindgen]
extern \"C\" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

fn console_log(value: f32) {
    console_log!(\"{}\", value);
}
";

const ts_func_def: &str = "
const console_log = (value: number) => {
    console.log(value);
};
";

pub const console_log: Lazy<BuiltInFunc> = Lazy::new(|| BuiltInFunc::new(
    "console_log",
    rust_func_def,
    ts_func_def,
    TypedAstType::Func(vec![TypedAstType::Number], Some(Box::new(TypedAstType::Void))),
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
