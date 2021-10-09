use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::type_parser::typed_ast::{TypedIfStmt, TypedCanElseStmt};
use std::borrow::Borrow;

impl ToRust {
    pub fn if_stmt_to_rust(&self, typed_if_stmt: TypedIfStmt) -> String {
        let condition_expr_str = self.expr_to_rust(typed_if_stmt.get_condition_expr());
        let then_stmt_str = self.block_box_to_rust(typed_if_stmt.get_then_stmt());
        let else_stmt_str = match typed_if_stmt.get_else_stmt() {
            Some(box_can_else_stmt) => {
                match box_can_else_stmt.borrow() {
                    TypedCanElseStmt::BlockBox(block_box) => {
                        Some(self.block_box_to_rust(block_box.clone()))
                    },
                    TypedCanElseStmt::IfStmt(if_stmt) => {
                        Some(self.if_stmt_to_rust(if_stmt.clone()))
                    }
                }
            },
            None => {None}
        };

        // TODO: return考える

        match else_stmt_str {
            Some(else_stmt_str) => {
                format!("if {} {}else {}", condition_expr_str, then_stmt_str, else_stmt_str)
            },
            None => {
                format!("if {} {}", condition_expr_str, then_stmt_str)
            }
        }

    }
}

#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedBlockBox, TypedCanElseStmt};
    use crate::to_ts_rust::to_rust::to_rust::ToRust;

    #[test]
    fn test_if_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(
                vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))],
                None
            ),
            None,
            None
        )];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "if true {1;}";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_if_else_block_box_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1.0, "1.0".to_string()),
                TypedExpr::num_expr_new(2.0, "2.0".to_string())
            ))], None),
            Some(TypedCanElseStmt::block_box_new(
                vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))],
                None
            )),
            None
        )];


        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "if true {1+2;}else {1;}";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_if_elseif_stmt() {
        let typed_stmts = vec![TypedStmt::if_stmt_new(
            TypedExpr::bool_expr_new(true),
            TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_add_new(
                TypedExpr::num_expr_new(1.0, "1.0".to_string()),
                TypedExpr::num_expr_new(2.0, "2.0".to_string())
            ))], None),
            Some(TypedCanElseStmt::if_stmt_new(
                TypedExpr::bool_expr_new(true),
                TypedBlockBox::new(vec![TypedStmt::expr_new(TypedExpr::num_expr_new(1.0, "1.0".to_string()))], None),
                None,
                None
            )),
            None
        )];


        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "if true {1+2;}else if true {1;}";

        assert_eq!(rust_code, expected_rust_code);

    }

}