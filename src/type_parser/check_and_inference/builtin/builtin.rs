#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Expr, Ident};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypedStmt, TypedFunc, TypedIdent, TypedFuncArg, TypedReturnStmt, TypedExpr, TypeFlag, TypedAstType, TypedCallExpr, TypedNumber};

    #[test]
    fn test_inference_console_log_call() {
        let stmts = vec![
            Stmt::expr_new(Expr::call_new(
                Ident::new("console_log".to_string()),
                vec![Expr::num_new(1.0, "1.0")],
            )),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(TypedExpr::CallExpr(
                TypedAstType::Void,
                TypedCallExpr::new(
                    TypedIdent::new("console_log".to_string()),
                    vec![
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1.0, "1.0".to_string()))
                    ],
                ),
            )),
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


    #[test]
    fn test_inference_performance_now_call() {
        let stmts = vec![
            Stmt::expr_new(Expr::call_new(
                Ident::new("performance_now".to_string()),
                vec![],
            )),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(TypedExpr::CallExpr(
                TypedAstType::Number,
                TypedCallExpr::new(
                    TypedIdent::new("performance_now".to_string()),
                    vec![],
                ),
            )),
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

}