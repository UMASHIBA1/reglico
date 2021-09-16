use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedIdent, TypedFunc, TypedReturnStmt, TypedCallExpr, TypedAstType, TypedNumber};
use std::collections::HashMap;
use crate::to_ts_rust::common_struct::CanAssignObj;

pub struct ToRust {
    pub var_env: HashMap<TypedIdent, Option<CanAssignObj>>,
}

impl ToRust {

    pub fn to_rust(typed_stmts: Vec<TypedStmt>, var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>) -> String {
        let mut rust_code = "".to_string();

        let mut to_rust = ToRust::new(var_env);
        for typed_stmt in typed_stmts {
            rust_code = format!("{}{}", rust_code, to_rust.stmt_to_rust(typed_stmt));
        };
        rust_code
    }
    // You must not publish new() method. Because some pub methods will publish to outer of this struct via struct instance, if you publish new() method.
    fn new(var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>) -> ToRust {
        match var_env {
            Some(var_env) => {
                ToRust {
                    var_env
                }
            },
            None => {
                ToRust {
                    var_env: HashMap::new(),
                }
            }
        }

    }

    fn stmt_to_rust(&mut self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_rust(var_decl),
            TypedStmt::ExprStmt(typed_expr) => format!("{};", self.expr_to_rust(typed_expr)),
            TypedStmt::Func(typed_func) =>  self.func_to_rust(typed_func),
            TypedStmt::ReturnStmt(return_stmt) => self.return_stmt_to_rust(&return_stmt),
        }
    }

    fn return_stmt_to_rust(&self, return_stmt: &TypedReturnStmt) -> String {
        let expr = return_stmt.get_expr();
            self.expr_to_rust(expr)
    }

}

#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedFunc, TypedFuncArg, TypeFlag, TypedReturnStmt, TypedAstType, TypedCallExpr, TypedNumber, TypedExpr, TypedVariableDeclaration};
    use crate::to_ts_rust::to_rust::to_rust::ToRust;
    use std::collections::HashMap;
    use crate::to_ts_rust::common_struct::CanAssignObj;

    #[test]
    fn test_return_stmt() {
        let typed_stmts = vec![
            TypedStmt::ReturnStmt(
                TypedReturnStmt::new(
                    TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0))
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "0";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_add_func() {
        let typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),
                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            ),
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new(
                        "total".to_string()),
                    None,
                    Some(TypedExpr::CallExpr(
                        TypedAstType::Number,
                        TypedCallExpr::new(
                            TypedIdent::new("add".to_string()),
                            vec![
                                TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                                TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                            ]
                        )
                    ))
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "fn add(a:i32,b:i32)->i32{a+b}let total=add(1,2);";

        assert_eq!(rust_code, expected_rust_code)


    }

}