use crate::settings::builtin::builtin_funcs::BuiltInFunc;
use once_cell::sync::Lazy;
use crate::type_parser::typed_ast::{TypedAstType, TypedFunc, TypedIdent, TypedStmt, TypedExpr};
use crate::to_ts_rust::common_struct::CanAssignObj;

const RUST_FUNC_DEF: &str = "
fn performance_now() -> i64 {
    let window = web_sys::window().expect(\"should have a window in this context\");
    let performance = window
        .performance()
        .expect(\"performance should be available\");

    let now_i64 = performance.now() as i64;
    now_i64
}
";

const TS_FUNC_DEF: &str = "
const performance_now = () => {
    return performance.now();
};
";

pub const PERFORMANCE_NOW: Lazy<BuiltInFunc> = Lazy::new(|| BuiltInFunc::new(
    "performance_now",
    RUST_FUNC_DEF,
    TS_FUNC_DEF,
    TypedAstType::Func(vec![], Box::new(TypedAstType::Number)),
    CanAssignObj::TypedFunc(TypedFunc::new(
        TypedIdent::new("performance_now".to_string()),
        vec![],
        vec![TypedStmt::expr_new(TypedExpr::call_expr_new(
            TypedAstType::Void,
            TypedIdent::new("performance_now".to_string()),
            vec![]
        ))],
        TypedAstType::Number
    ))));
