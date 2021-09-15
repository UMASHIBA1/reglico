use crate::parser::ast::{Stmt, Expr, Ident, Types, Opcode, Func, ReturnStmt, CallExpr, Operation};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedExpr, TypeFlag, TypedNumber, TypedAstType, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};
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

    fn check_and_inference_func(&mut self, func: Func) -> TypedFunc {
        let name = self.convert_ident_to_typed_ident(func.get_name());
        let args = func.get_func_args();
        let stmts =  func.get_stmts();
        let mut arg_typed_ast_type = vec![];

        // NOTE: check_and_inference args type
        let mut typed_args = vec![];
        for arg in args {
            let arg_type = arg.get_arg_type();
            let typed_arg_type = self.convert_type_to_typed_type(arg_type.clone());

            arg_typed_ast_type.push(
                self.convert_type_to_typed_ast_type(arg_type)
            );

            typed_args.push(
                TypedFuncArg::new(
                    self.convert_ident_to_typed_ident(arg.get_name()),
                    typed_arg_type,
                )
            );
        };

        let mut func_type_env = self.type_env.clone();
        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            func_type_env.insert(typed_arg.get_name(), arg_typed_ast_type.get(i).unwrap().clone());
        };

        let mut func_stmts = TypeCheckAndInference::check_and_inference(stmts, Some(&func_type_env));
        let mut typed_return_stmt: Option<TypedReturnStmt> = None;
        let mut return_stmt_index: Option<usize> = None;
        for (i, stmt) in func_stmts.iter().enumerate() {
            match stmt {
                TypedStmt::ReturnStmt(_) => {
                    return_stmt_index = Some(i);
                },
                _ => {}
            }
        }
        match return_stmt_index {
            Some(i) => {
                let will_typed_return_stmt = func_stmts.remove(i);
                match will_typed_return_stmt {
                    TypedStmt::ReturnStmt(return_stmt) => {
                        typed_return_stmt = Some(return_stmt);
                    }
                    _ => {},
                };
            },
            _ => {}
        }

        let return_typed_ast_type = {
            match &typed_return_stmt {
                Some(typed_return_stmt) => {
                    Some(Box::new(typed_return_stmt.get_expr().get_typed_ast_type()))
                },
                None => Some(Box::new(TypedAstType::Void)),
            }
        };

        self.type_env.insert(name.clone(), TypedAstType::Func(arg_typed_ast_type, return_typed_ast_type));

        TypedFunc::new(
            name,
            typed_args,
            func_stmts,
            typed_return_stmt
        )

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

    fn convert_type_to_typed_ast_type(&self, type_flag: Types) -> TypedAstType {
        match type_flag {
            Types::NumberType => TypedAstType::Number,
            _ => TypedAstType::Func(vec![TypedAstType::Number],Some(Box::new(TypedAstType::Number))) // TODO: 適当に書いた、後で直す
        }
    }

}