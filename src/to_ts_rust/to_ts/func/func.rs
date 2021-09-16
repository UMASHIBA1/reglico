use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::type_parser::typed_ast::{TypedAstType, TypedExpr, TypedFunc, TypedNumber, TypedStmt};

impl ToTs {
    pub fn func_to_ts(&mut self, typed_func: TypedFunc) -> String {
        let name = typed_func.get_name();
        let args = typed_func.get_args();
        let stmts = typed_func.get_stmts();
        let return_stmt = typed_func.get_return_stmt();

        self.var_env
            .insert(name.clone(), Some(CanAssignObj::TypedFunc(typed_func)));

        let mut func_var_env = self.var_env.clone();
        for arg in &args {
            func_var_env.insert(
                arg.get_name(),
                Some(CanAssignObj::TypedExpr(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(0),
                ))),
            );
        }

        let args_str = {
            let mut args_str: String;
            let first_arg = &args.get(0);
            match first_arg {
                Some(first_arg) => {
                    args_str = format!(
                        "{}:{}",
                        first_arg.get_name().get_name(),
                        self.type_flag_to_ts(first_arg.get_arg_type())
                    );
                    let mut args_iter = args.iter();
                    args_iter.next();

                    for typed_func_arg in args_iter {
                        let arg = format!(
                            "{}:{}",
                            typed_func_arg.get_name().get_name(),
                            self.type_flag_to_ts(typed_func_arg.get_arg_type())
                        );
                        args_str = format!("{},{}", args_str, arg);
                    }
                }
                None => {
                    args_str = "".to_string();
                }
            }
            args_str
        };

        let stmts_str = ToTs::to_ts(stmts, Some(func_var_env.clone()));

        let (return_type, return_stmt_str) = {
            match &return_stmt {
                Some(typed_return_stmt) => {
                    let return_type =
                        self.typed_ast_type_to_ts(typed_return_stmt.get_return_type());
                    let return_stmt_str = ToTs::to_ts(
                        vec![TypedStmt::ReturnStmt(typed_return_stmt.clone())],
                        Some(func_var_env),
                    );
                    (return_type, Some(return_stmt_str))
                }
                None => ("void".to_string(), None),
            }
        };

        match return_stmt_str {
            Some(return_stmt_str) => {
                format!(
                    "const {}=({}):{}=>{{{}{}}}",
                    name.get_name(),
                    args_str,
                    return_type,
                    stmts_str,
                    return_stmt_str
                )
            }
            None => {
                format!(
                    "const {}=({}):void=>{{{}}}",
                    name.get_name(),
                    args_str,
                    stmts_str
                )
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::to_ts_rust::to_ts::to_ts::ToTs;
    use crate::type_parser::typed_ast::{
        TypeFlag, TypedAstType, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent, TypedReturnStmt,
        TypedStmt,
    };

    #[test]
    fn test_func_declaration() {
        let typed_stmts = vec![TypedStmt::Func(TypedFunc::new(
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
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "const add=(a:number,b:number):number=>{return a+b;}";

        assert_eq!(ts_code, expected_ts_code);
    }
}
