use crate::parser::ast::Stmt;
use crate::type_parser::typed_ast::TypedStmt;
use crate::type_parser::check_and_inference::_struct::TypeCheckAndInference;

pub fn check_and_inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeCheckAndInference::check_and_inference(stmts, None)
}
