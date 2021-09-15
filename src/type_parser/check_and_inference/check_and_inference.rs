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




}
