use crate::type_parser::typed_ast::{TypedExpr, TypedFunc};

#[derive(Debug, Clone)]
pub enum CanAssignObj {
    TypedFunc(TypedFunc),
    TypedExpr(TypedExpr),
}
