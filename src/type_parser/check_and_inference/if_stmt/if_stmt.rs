use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{TypedIfStmt, TypedCanElseStmt};
use crate::parser::ast::{IfStmt, CanElseStmt};
use std::borrow::Borrow;

impl TypeCheckAndInference {
    pub fn check_and_inference_if_stmt(&self, if_stmt: IfStmt) -> TypedIfStmt {
        let condition_expr = self.check_and_inference_expr(if_stmt.get_condition_expr());
        let then_stmt = self.check_and_inference_block_box(if_stmt.get_then_stmt());
        let else_stmt: Option<TypedCanElseStmt> = match if_stmt.get_else_stmt() {
            Some(can_else_stmt) => {
                match can_else_stmt.borrow() {
                    CanElseStmt::BlockBox(block_box) => {
                        Some(TypedCanElseStmt::BlockBox(self.check_and_inference_block_box(block_box.clone())))
                    },
                    CanElseStmt::IfStmt(if_stmt) => {
                        Some(TypedCanElseStmt::IfStmt(self.check_and_inference_if_stmt(if_stmt.clone())))
                    }
                }
            },
            None => None
        };

        // TODO: union型ができたらelse_stmtの型もとる
        let return_typed_ast = then_stmt.get_return_ast_type();

        TypedIfStmt::new(condition_expr, then_stmt, else_stmt, return_typed_ast)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Expr, BlockBox, Opcode, CanElseStmt};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedBlockBox, TypedCanElseStmt, TypedAstType};

    #[test]
    fn test_inference_if_else_block_stmt() {
        // if(true){1 + 2;}else{1;}
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(vec![Stmt::expr_new(Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2")))]),
            Some(CanElseStmt::block_box_new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]))
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1, "1".to_string()),
                TypedExpr::num_expr_new(2, "2".to_string())
            ))], TypedAstType::Void),
            Some(TypedCanElseStmt::block_box_new(
                vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1, "1".to_string()))],
                TypedAstType::Void
            )),
            TypedAstType::Void
        )];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_inference_if_elseif_stmt() {
        // if(true){1 + 2;}else if(true){1;}
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(vec![Stmt::expr_new(Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2")))]),
            Some(CanElseStmt::if_stmt_new(
                Expr::bool_new(true),
                BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                None,
            ))
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1, "1".to_string()),
                TypedExpr::num_expr_new(2, "2".to_string())
            ))], TypedAstType::Void),
            Some(TypedCanElseStmt::if_stmt_new(
                TypedExpr::bool_expr_new(true),
                TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1, "1".to_string()))], TypedAstType::Void),
                None,
                TypedAstType::Void
            )),
            TypedAstType::Void
        )];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_inference_if_stmt() {
        // if(true){1 + 2;}
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(vec![Stmt::expr_new(Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2")))]),
            None
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1, "1".to_string()),
                TypedExpr::num_expr_new(2, "2".to_string())
            ))], TypedAstType::Void),
            None,
            TypedAstType::Void
        )];

        assert_eq!(typed_stmts, expected_typed_stmts);

    }

    #[test]
    fn test_inference_if_stmt_with_return_stmt() {
        // if(true){return 1 + 2;}
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(
                vec![Stmt::return_new(
                    Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2"))
                )]
            ),
            None
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::return_new(
                TypedExpr::num_add_new(
                    TypedExpr::num_expr_new(1, "1".to_string()),
                    TypedExpr::num_expr_new(2, "2".to_string())
                )
            )], TypedAstType::Number),
            None,
            TypedAstType::Number
        )];

        assert_eq!(typed_stmts, expected_typed_stmts);

    }


    #[test]
    fn test_inference_if_stmt_with_return_stmt_in_if_stmt() {
        // if(true){if(true){return 1 + 2;}}
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(
                vec![
                    Stmt::if_stmt(
                        Expr::bool_new(true),
                        BlockBox::new(vec![
                            Stmt::return_new(
                                Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2"))
                            ),
                        ]),
                        None),
                ]
            ),
            None
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(
                vec![
                    TypedStmt::if_stmt_new(
                        TypedExpr::bool_expr_new(true),
                        TypedBlockBox::new(
                            vec![
                                TypedStmt::return_new(
                                    TypedExpr::num_add_new(
                                    TypedExpr::num_expr_new(1, "1".to_string()),
                                    TypedExpr::num_expr_new(2, "2".to_string()),
                                    )
                                )
                            ],
                            TypedAstType::Number
                        ),
                        None,
                        TypedAstType::Number
                    ),
                ],
                TypedAstType::Number),
            None,
            TypedAstType::Number
        )];

        assert_eq!(typed_stmts, expected_typed_stmts);

    }

}