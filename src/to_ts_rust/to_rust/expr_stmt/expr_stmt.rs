use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::type_parser::typed_ast::{TypedExpr, TypedCallExpr};
use crate::to_ts_rust::common_struct::CanAssignObj;

impl ToRust {

    pub fn expr_to_rust(&self, typed_expr: TypedExpr) -> String {
        match typed_expr {
            TypedExpr::NumExpr(_, num) =>  format!("{}", num.get_num()),
            TypedExpr::NumIdentExpr(_, ident) => {
                if self.is_exist_ident(&ident) {
                    ident.get_name()
                } else {
                    panic!("specified ident `{}` does not defined or initialized.", ident.get_name());
                }
            },
            TypedExpr::NumAddExpr(_, l, r) => format!("{}+{}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            TypedExpr::NumSubExpr(_, l, r) => format!("{}-{}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            TypedExpr::NumMulExpr(_, l, r) => format!("{}*{}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            TypedExpr::NumDivExpr(_, l, r) => format!("{}/{}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            TypedExpr::CallExpr(_, call) => self.call_expr_to_rust(call)
        }
    }

    fn call_expr_to_rust(&self, typed_call_expr: TypedCallExpr) -> String {
        let func_name = typed_call_expr.get_func_name();
        if self.is_exist_ident(&func_name) {
            let ident_value = self.var_env.get(&func_name);
            match ident_value {
                Some(can_assign_obj) => {
                    match can_assign_obj {
                        Some(can_assign_obj) => {
                            match can_assign_obj {
                                CanAssignObj::TypedFunc(_) => {
                                    let args = {
                                        let args = typed_call_expr.get_args();
                                        let mut str_args: String;

                                        let first_arg = args.get(0);
                                        let mut args_iter = args.iter();
                                        args_iter.next();
                                        match first_arg {
                                            Some(arg) => {
                                                str_args = self.expr_to_rust(arg.clone());
                                                for arg in args_iter {
                                                    str_args = format!("{},{}", str_args, self.expr_to_rust(arg.clone()));
                                                }
                                                str_args
                                            },
                                            None => "".to_string()
                                        }

                                    };

                                    format!("{}({})", func_name.get_name(), args)
                                },
                                _ => panic!("specified variable `{}` does not func. this is {:?}", func_name.get_name(), can_assign_obj)
                            }
                        },
                        None => panic!("specified func `{}` does not defined or initialized", func_name.get_name())
                    }
                },
                None => panic!("specified func `{}` does not defined or initialized", func_name.get_name())
            }
        } else {
            panic!("specified func `{}` does not defined or initialized", func_name.get_name());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedExpr, TypedAstType, TypedNumber, TypedIdent, TypedCallExpr, TypedFunc, TypedFuncArg, TypeFlag, TypedReturnStmt};
    use crate::to_ts_rust::to_rust::to_rust::ToRust;
    use std::collections::HashMap;
    use crate::to_ts_rust::common_struct::CanAssignObj;

    #[test]
    fn test_num_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0)))
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "0;";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_num_ident_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("tmp1".to_string())))
        ];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(TypedIdent::new(
            "tmp1".to_string()),
                       Some(CanAssignObj::TypedExpr(
                           TypedExpr::NumExpr(
                               TypedAstType::Number,
                               TypedNumber::new(0)
                           )
                       ))
        );

        let rust_code = ToRust::to_rust(typed_stmts, Some(var_env));

        let expected_rust_code = "tmp1;";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_num_add_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(1)
                        )
                    ),
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(2)
                        )
                    )
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "1+2;";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_num_sub_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumSubExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(2)
                        )
                    ),
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(1)
                        )
                    )
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "2-1;";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_num_mul_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumMulExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(2)
                        )
                    ),
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(1)
                        )
                    )
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "2*1;";

        assert_eq!(rust_code, expected_rust_code);

    }

    #[test]
    fn test_num_div_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumDivExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(4)
                        )
                    ),
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(2)
                        )
                    )
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "4/2;";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_num_multi_op_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(
                        TypedExpr::NumMulExpr(
                            TypedAstType::Number,
                            Box::new(
                                TypedExpr::NumExpr(
                                    TypedAstType::Number,
                                    TypedNumber::new(2)
                                )
                            ),
                            Box::new(
                                TypedExpr::NumExpr(
                                    TypedAstType::Number,
                                    TypedNumber::new(3)
                                )
                            )
                        )
                    ),
                    Box::new(
                        TypedExpr::NumExpr(
                            TypedAstType::Number,
                            TypedNumber::new(2)
                        )
                    )
                )
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "2*3+2;";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_call_expr_stmt() {
        let typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::CallExpr(
                    TypedAstType::Number,
                    TypedCallExpr::new(
                        TypedIdent::new("add".to_string()),
                        vec![
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                        ]
                    )
                )
            )
        ];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(TypedIdent::new(
            "add".to_string()),
                       Some(CanAssignObj::TypedFunc(
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
                       ))
        );

        let rust_code = ToRust::to_rust(typed_stmts, Some(var_env));

        let expected_rust_code = "add(1,2);";

        assert_eq!(rust_code, expected_rust_code);

    }

}