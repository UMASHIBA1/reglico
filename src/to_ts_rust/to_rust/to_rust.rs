use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::type_parser::typed_ast::{TypedIdent, TypedReturnStmt, TypedStmt};
use std::collections::HashMap;

pub struct ToRust {
    pub var_env: HashMap<TypedIdent, Option<CanAssignObj>>,
}

impl ToRust {
    pub fn to_rust(
        typed_stmts: Vec<TypedStmt>,
        var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>,
    ) -> String {
        let mut rust_code = "".to_string();

        let mut to_rust = ToRust::new(var_env);
        for typed_stmt in typed_stmts {
            rust_code = format!("{}{}", rust_code, to_rust.stmt_to_rust(typed_stmt));
        }
        rust_code
    }
    // You must not publish new() method. Because some pub methods will publish to outer of this struct via struct instance, if you publish new() method.
    fn new(var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>) -> ToRust {
        match var_env {
            Some(var_env) => ToRust { var_env },
            None => ToRust {
                var_env: HashMap::new(),
            },
        }
    }

    fn stmt_to_rust(&mut self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_rust(var_decl),
            TypedStmt::ExprStmt(typed_expr) => format!("{};", self.expr_to_rust(typed_expr)),
            TypedStmt::Func(typed_func) => self.func_to_rust(typed_func),
            TypedStmt::ReturnStmt(return_stmt) => self.return_stmt_to_rust(&return_stmt),
            TypedStmt::IfStmt(if_stmt) => self.if_stmt_to_rust(if_stmt)
        }
    }

    fn return_stmt_to_rust(&self, return_stmt: &TypedReturnStmt) -> String {
        let expr = return_stmt.get_expr();
        self.expr_to_rust(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::to_ts_rust::to_rust::to_rust::ToRust;
    use crate::type_parser::typed_ast::{
        TypeFlag, TypedAstType, TypedCallExpr, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent,
        TypedNumber, TypedReturnStmt, TypedStmt, TypedVariableDeclaration,
    };

    #[test]
    fn test_return_stmt() {
        let typed_stmts = vec![TypedStmt::ReturnStmt(TypedReturnStmt::new(
            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0.0)),
        ))];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "0";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_add_func() {
        let typed_stmts = vec![
            TypedStmt::Func(TypedFunc::new(
                TypedIdent::new("add".to_string()),
                vec![
                    TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                    TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                ],
                vec![],
                Some(TypedReturnStmt::new(TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumIdentExpr(
                        TypedAstType::Number,
                        TypedIdent::new("a".to_string()),
                    )),
                    Box::new(TypedExpr::NumIdentExpr(
                        TypedAstType::Number,
                        TypedIdent::new("b".to_string()),
                    )),
                ))),
            )),
            TypedStmt::VariableDeclaration(TypedVariableDeclaration::new(
                TypedIdent::new("total".to_string()),
                None,
                Some(TypedExpr::CallExpr(
                    TypedAstType::Number,
                    TypedCallExpr::new(
                        TypedIdent::new("add".to_string()),
                        vec![
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1.0)),
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2.0)),
                        ],
                    ),
                )),
            )),
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "fn add(a:f32,b:f32)->f32{a+b}let total=add(1,2);";

        assert_eq!(rust_code, expected_rust_code)
    }
}
