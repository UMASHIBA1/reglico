use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::type_parser::typed_ast::{TypedIdent, TypedReturnStmt, TypedStmt};
use std::collections::HashMap;

pub struct ToTs {
    pub var_env: HashMap<TypedIdent, Option<CanAssignObj>>,
}

impl ToTs {
    pub fn to_ts(
        typed_stmts: Vec<TypedStmt>,
        var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>,
    ) -> String {
        let mut js_code = "".to_string();

        let mut to_js = ToTs::new(var_env);
        for typed_stmt in typed_stmts {
            js_code = format!("{}{}", js_code, to_js.stmt_to_ts(typed_stmt));
        }
        js_code
    }

    // You must not publish new() method. Because some pub methods will publish to outer of this struct via struct instance, if you publish new() method.
    fn new(var_env: Option<HashMap<TypedIdent, Option<CanAssignObj>>>) -> ToTs {
        match var_env {
            Some(var_env) => ToTs { var_env },
            None => ToTs {
                var_env: HashMap::new(),
            },
        }
    }

    fn stmt_to_ts(&mut self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_ts(var_decl),
            TypedStmt::ExprStmt(typed_expr) => format!("{};", self.expr_to_ts(typed_expr)),
            TypedStmt::Func(typed_func) => self.func_to_ts(typed_func),
            TypedStmt::ReturnStmt(return_stmt) => self.return_stmt_to_ts(&return_stmt),
            TypedStmt::IfStmt(if_stmt) => self.if_stmt_to_ts(if_stmt),
        }
    }

    fn return_stmt_to_ts(&self, return_stmt: &TypedReturnStmt) -> String {
        let expr_str = self.expr_to_ts(return_stmt.get_expr());
        format!("return {};", expr_str)
    }
}

#[cfg(test)]
mod tests {
    use crate::to_ts_rust::to_ts::to_ts::ToTs;
    use crate::type_parser::typed_ast::{
        TypeFlag, TypedAstType, TypedCallExpr, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent,
        TypedNumber, TypedReturnStmt, TypedStmt, TypedVariableDeclaration,
    };

    #[test]
    fn test_return_stmt() {
        let typed_stmts = vec![TypedStmt::ReturnStmt(TypedReturnStmt::new(
            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0)),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "return 0;";

        assert_eq!(ts_code, expected_ts_code);
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
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                        ],
                    ),
                )),
            )),
        ];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code =
            "const add=(a:number,b:number):number=>{return a+b;};const total=add(1,2);";

        assert_eq!(ts_code, expected_ts_code);
    }
}
