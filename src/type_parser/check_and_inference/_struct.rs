use crate::parser::ast::{Stmt, Ident, Types, ReturnStmt};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypeFlag, TypedAstType, TypedReturnStmt};
use std::collections::HashMap;

pub struct TypeCheckAndInference {
    pub type_env: HashMap<TypedIdent, TypedAstType>
}

impl TypeCheckAndInference {
    pub fn check_and_inference(stmts: Vec<Stmt>, type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> Vec<TypedStmt>{
        let mut type_inference = TypeCheckAndInference::new(type_env);
        let mut typed_stmts = vec![];
        for stmt in stmts {
            typed_stmts.push(type_inference.check_and_inference_a_stmt(stmt));
        };
        typed_stmts
    }

    // You must not publish new() method. Because some pub methods will publish to outer of this struct via struct instance, if you publish new() method.
    fn new(type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> TypeCheckAndInference {
        TypeCheckAndInference {
            type_env: match type_env {
                Some(env) => env.clone(),
                None => HashMap::new(),
            }
        }
    }

    fn check_and_inference_a_stmt(&mut self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => TypedStmt::VariableDeclaration(self.check_and_inference_var_declaration(var_decl)),
            Stmt::ExprStmt(expr_stmt) => TypedStmt::ExprStmt(self.check_and_inference_expr(expr_stmt.get_expr())),
            Stmt::Func(func) => TypedStmt::Func(self.check_and_inference_func(func)),
            Stmt::ReturnStmt(return_stmt) => TypedStmt::ReturnStmt(self.convert_return_stmt_to_typed_return_stmt(return_stmt)),
        }
    }



    fn convert_return_stmt_to_typed_return_stmt(&self, return_stmt: ReturnStmt) -> TypedReturnStmt {
        TypedReturnStmt::new(
            self.check_and_inference_expr(return_stmt.get_expr()),
        )
    }


    pub fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    pub fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType
        }
    }

    pub fn convert_type_to_typed_ast_type(&self, type_flag: Types) -> TypedAstType {
        match type_flag {
            Types::NumberType => TypedAstType::Number,
            _ => TypedAstType::Func(vec![TypedAstType::Number],Some(Box::new(TypedAstType::Number))) // TODO: 適当に書いた、後で直す
        }
    }

}