use crate::type_parser::typed_ast::{TypedFunc, TypedExpr, TypedIdent};
use std::collections::HashMap;

#[derive(Debug)]
pub enum CanAssignObj {
    TypedFunc(TypedFunc),
    TypedExpr(TypedExpr),
}
