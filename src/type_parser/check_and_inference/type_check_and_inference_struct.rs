use crate::parser::ast::{ReturnStmt, Stmt};
use crate::type_parser::typed_ast::{TypedAstType, TypedIdent, TypedReturnStmt, TypedStmt};
use std::collections::HashMap;
use crate::settings::builtin::builtin_funcs::BUILTIN_FUNCS;

pub struct TypeCheckAndInference {
    pub type_env: HashMap<TypedIdent, TypedAstType>,
}

impl TypeCheckAndInference {
    pub fn check_and_inference(
        stmts: Vec<Stmt>,
        type_env: Option<&HashMap<TypedIdent, TypedAstType>>,
    ) -> Vec<TypedStmt> {
        let mut type_inference = TypeCheckAndInference::new(type_env);
        let mut typed_stmts = vec![];

        // add builtin to type_env
        for func in BUILTIN_FUNCS {
            type_inference.type_env.insert(TypedIdent::new(func.get_name().to_string()), func.get_typed_ast_type());
        }

        for stmt in stmts {
            typed_stmts.push(type_inference.check_and_inference_a_stmt(stmt));
        }
        typed_stmts
    }

    // You must not publish new() method. Because some pub methods will publish to outer of this struct via struct instance, if you publish new() method.
    fn new(type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> TypeCheckAndInference {
        TypeCheckAndInference {
            type_env: match type_env {
                Some(env) => env.clone(),
                None => HashMap::new(),
            },
        }
    }

    fn check_and_inference_a_stmt(&mut self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => {
                TypedStmt::VariableDeclaration(self.check_and_inference_var_declaration(var_decl))
            }
            Stmt::ExprStmt(expr_stmt) => {
                TypedStmt::ExprStmt(self.check_and_inference_expr(expr_stmt.get_expr()))
            }
            Stmt::Func(func) => TypedStmt::Func(self.check_and_inference_func(func)),
            Stmt::ReturnStmt(return_stmt) => {
                TypedStmt::ReturnStmt(self.convert_return_stmt_to_typed_return_stmt(return_stmt))
            },
            Stmt::IfStmt(if_stmt) => TypedStmt::IfStmt(self.check_and_inference_if_stmt(if_stmt)),
        }
    }

    fn convert_return_stmt_to_typed_return_stmt(&self, return_stmt: ReturnStmt) -> TypedReturnStmt {
        TypedReturnStmt::new(self.check_and_inference_expr(return_stmt.get_expr()))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Expr};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypedStmt, TypedReturnStmt, TypedExpr, TypedAstType, TypedNumber};

    #[test]
    fn test_check_and_inference_return_stmt() {
        let stmts = vec![Stmt::return_new(Expr::num_new(1, "1"))];

        let typed_stmt = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ReturnStmt(
            TypedReturnStmt::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1, "1".to_string()),
            ))
        )];

        assert_eq!(typed_stmt, expected_typed_stmts)

    }

}
