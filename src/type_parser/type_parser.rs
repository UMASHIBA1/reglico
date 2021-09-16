use crate::parser::ast::Stmt;
use crate::type_parser::typed_ast::TypedStmt;
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;

pub fn type_parser(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeCheckAndInference::check_and_inference(stmts, None)
}

