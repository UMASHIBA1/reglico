#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedAstType, TypedIdent};
    use crate::to_ts_rust::to_ts::to_ts::ToTs;

    #[test]
    fn test_console_log_call() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::call_expr_new(
            TypedAstType::Void,
            TypedIdent::new("console_log".to_string()),
            vec![TypedExpr::num_expr_new(1.0, "1.0".to_string())]
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "console_log(1);";

        assert_eq!(ts_code, expected_ts_code);

    }

    #[test]
    fn test_performance_now_call() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::call_expr_new(
            TypedAstType::Number,
            TypedIdent::new("performance_now".to_string()),
            vec![]
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "performance_now();";

        assert_eq!(ts_code, expected_ts_code);

    }
}