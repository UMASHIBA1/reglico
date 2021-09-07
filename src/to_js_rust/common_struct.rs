use crate::type_parser::typed_ast::{TypedFunc, TypedExpr};

#[derive(Debug, Clone)]
pub enum CanAssignObj {
    TypedFunc(TypedFunc),
    TypedExpr(TypedExpr),
}
