use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::type_parser::typed_ast::{TypedIfStmt, TypedCanElseStmt};
use std::borrow::Borrow;

impl ToTs {
    pub fn if_stmt_to_ts(&self, typed_if_stmt: TypedIfStmt) -> String {
        let condition_expr_str = self.expr_to_ts(typed_if_stmt.get_condition_expr());
        let then_stmt_str = self.block_box_to_ts(typed_if_stmt.get_then_stmt());
        let else_stmt_str = match typed_if_stmt.get_else_stmt() {
            Some(box_can_else_stmt) => {
                match box_can_else_stmt.borrow() {
                    TypedCanElseStmt::BlockBox(block_box) => {
                        Some(self.block_box_to_ts(block_box.clone()))
                    },
                    TypedCanElseStmt::IfStmt(if_stmt) => {
                        Some(self.if_stmt_to_ts(if_stmt.clone()))
                    }
                }
            },
            None => {None}
        };

        match else_stmt_str {
            Some(else_stmt_str) => {
                format!("if({}){}else {}", condition_expr_str, then_stmt_str, else_stmt_str)
            },
            None => {
                format!("if({}){}", condition_expr_str, then_stmt_str)
            }
        }

    }
}


#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedBlockBox, TypedCanElseStmt, TypedAstType};
    use crate::to_ts_rust::to_ts::to_ts::ToTs;

    #[test]
    fn test_if_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(
                vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))],
                TypedAstType::Void
            ),
            None,
            TypedAstType::Void
        )];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "if(true){1;}";

        assert_eq!(ts_code, expected_ts_code);

    }

    #[test]
    fn test_if_else_block_box_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1.0, "1.0".to_string()),
                TypedExpr::num_expr_new(2.0, "2.0".to_string())
            ))], TypedAstType::Void),
            Some(TypedCanElseStmt::block_box_new(
                vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))],
                TypedAstType::Void
            )),
            TypedAstType::Void
        )];


        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "if(true){1+2;}else {1;}";

        assert_eq!(ts_code, expected_ts_code);

    }

    #[test]
    fn test_if_elseif_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1.0, "1.0".to_string()),
                TypedExpr::num_expr_new(2.0, "2.0".to_string())
            ))], TypedAstType::Void),
            Some(TypedCanElseStmt::if_stmt_new(
                TypedExpr::bool_expr_new(true),
                TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))], TypedAstType::Void),
                None,
                TypedAstType::Void
            )),
            TypedAstType::Void
        )];


        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "if(true){1+2;}else if(true){1;}";

        assert_eq!(ts_code, expected_ts_code);

    }


}