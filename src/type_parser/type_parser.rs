use crate::parser::ast::Stmt;
use crate::type_parser::typed_ast::TypedStmt;
use crate::type_parser::check_and_inference::check_and_inference::check_and_inference;

pub fn type_parser(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    check_and_inference(stmts)
}

