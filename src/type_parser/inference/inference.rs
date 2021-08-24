use crate::parser::ast::{Stmt, Expr, VariableDeclaration, Ident, Types};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedNumber, TypedAstType, TypedFunc, TypedReturnStmt, TypedFuncArg, TypedCallExpr};
use std::collections::HashMap;

// inference したいところ ->
pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeInference::inference(stmts)
}

enum AbleAstNode {
    TypedFunc(TypedFunc),
    TypedReturnStmt(TypedReturnStmt),
    TypedFuncArg(TypedFuncArg),
    TypedVariableDeclaration(TypedVariableDeclaration),
    TypedExpr(TypedExpr),
    CallExpr(TypedCallExpr),
    NumExpr(TypedNumber),
    NumIdentExpr(TypedIdent),
    NumAddExpr(TypedExpr),

}

struct TypeRelationAst {
    ast: AbleAstNode,
    typed_ast_type: RelationAstType
}

enum RelationAstType {
    TypedAstType(TypedAstType),
    RelationType(Box<TypeRelationAst>),
}

struct TypeInference {
    type_relation_ast: Vec<TypeRelationAst>,
}

impl TypeInference {
    pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt>{
        let type_inference = TypeInference::new();
        let mut typed_stmts = vec![];
        for stmt in stmts {
            typed_stmts.push(type_inference.inference_a_stmt(stmt));
        };
        typed_stmts
    }

    fn new() -> TypeInference {
        TypeInference {
            type_relation_ast: vec![],
        }
    }

    fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType
        }
    }

}


#[cfg(test)]
mod tests {
    #[test]
    fn test_inference_var_declaration(){

    }
}