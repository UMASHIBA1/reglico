use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{TypedIfStmt, TypedStmt, TypedBlockBox, TypedCanElseStmt};
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


        let return_stmt = {
            then_stmt.get_return_stmt()
            // TODO: elseの方も取得する
        };

        TypedIfStmt::new(condition_expr, then_stmt, else_stmt, return_stmt)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Expr, BlockBox, Number, Opcode};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedBlockBox};

    #[test]
    fn test_inference_if_stmt() {
        let stmts = vec![Stmt::if_stmt(
            Expr::bool_new(true),
            BlockBox::new(vec![Stmt::expr_new(Expr::op_new(Expr::num_new(1), Opcode::Add, Expr::num_new(2)))]),
            None
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1),
                TypedExpr::num_expr_new(2)
            ))], None),
            None,
            None
        )];

    }
}