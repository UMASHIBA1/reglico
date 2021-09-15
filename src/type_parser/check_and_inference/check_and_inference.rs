use crate::parser::ast::Stmt;
use crate::type_parser::typed_ast::TypedStmt;
use crate::type_parser::check_and_inference::_struct::TypeCheckAndInference;

pub fn check_and_inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeCheckAndInference::check_and_inference(stmts, None)
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Ident, Types, Expr, Opcode, FuncArg, ReturnStmt};
    use crate::type_parser::check_and_inference::check_and_inference::check_and_inference;
    use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedIdent, TypeFlag, TypedExpr, TypedAstType, TypedNumber, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};

    #[test]
    fn test_inference_num_add_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Add,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_sub_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Sub,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumSubExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_mul_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Mul,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumMulExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_div_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Div,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumDivExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_multi_op_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(

                Expr::op_new(
                    Expr::op_new(
                        Expr::num_new(1),
                        Opcode::Add,
                        Expr::num_new(2),
                    ),
                    Opcode::Mul,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumMulExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumAddExpr(
                            TypedAstType::Number,
                            Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                            Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                        )
                    ),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_ident_expr_stmt(){
        let stmts = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::NumberType),
                Some(Expr::num_new(10))
            ),
            Stmt::expr_new(Expr::ident_new(Ident::new("tmp1".to_string())))
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new("tmp1".to_string()),
                    Some(TypeFlag::NumberType),
                    Some(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(10)))
                )
            ),
            TypedStmt::ExprStmt(
                TypedExpr::NumIdentExpr(
                    TypedAstType::Number,
                    TypedIdent::new("tmp1".to_string()),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_func(){
        let stmts = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![Stmt::ReturnStmt(
                    ReturnStmt::new(
                    Expr::op_new(
                        Expr::ident_new(Ident::new("a".to_string())),
                        Opcode::Add,
                        Expr::ident_new(Ident::new("b".to_string())),
                    )
                ))
                ],
            ),
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),

                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_call(){
        let stmts = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![Stmt::ReturnStmt(
                        ReturnStmt::new(
                            Expr::op_new(
                                Expr::ident_new(Ident::new("a".to_string())),
                                Opcode::Add,
                                Expr::ident_new(Ident::new("b".to_string())),
                            )
                        )
                )],

            ),
            Stmt::expr_new(
                Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)]
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),

                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            ),
            TypedStmt::ExprStmt(
                TypedExpr::CallExpr(
                    TypedAstType::Number,
                    TypedCallExpr::new(
                        TypedIdent::new("add".to_string()),
                        vec![
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                        ]
                    )
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


}
