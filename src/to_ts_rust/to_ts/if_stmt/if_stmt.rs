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

        // TODO: return考える

        match else_stmt_str {
            Some(else_stmt_str) => {
                format!("if({}){}else{}", condition_expr_str, then_stmt_str, else_stmt_str)
            },
            None => {
                format!("if({}){}", condition_expr_str, then_stmt_str)
            }
        }

    }
}
