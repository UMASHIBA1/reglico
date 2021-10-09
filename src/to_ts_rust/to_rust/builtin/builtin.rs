#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedAstType, TypedCallExpr, TypedIdent};
    use crate::to_ts_rust::to_rust::to_rust::ToRust;

    #[test]
    fn test_console_log_call() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::call_expr_new(
            TypedAstType::Number,
            TypedIdent::new("console_log".to_string()),
            vec![TypedExpr::num_expr_new(1.0, "1.0".to_string())]
        ))];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "console_log(1.0);";

        assert_eq!(rust_code, expected_rust_code);

    }
}