use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::type_parser::typed_ast::{TypedCallExpr, TypedExpr, TypedBlock, TypedAstType};

impl ToTs {
    pub fn expr_to_ts(&self, typed_expr: TypedExpr) -> String {
        match typed_expr {
            TypedExpr::NumExpr(_, num) => format!("{}", num.get_num()),
            TypedExpr::BoolExpr(_, bool) => format!("{}", bool.get_bool()),
            TypedExpr::NumIdentExpr(_, ident) => {
                let ident_value = self.var_env.get(&ident);

                match ident_value {
                    Some(Some(can_assign_obj)) => {
                        match can_assign_obj {
                            CanAssignObj::TypedExpr(TypedExpr::NumExpr(..)) => {
                                ident.get_name()
                            },
                            _ => panic!("specified ident `{}` is not number type. this type is {:?}", ident.get_name(), can_assign_obj)
                        }
                    }
                    _ => {
                        panic!("specified ident `{}` does not defined or initialized", ident.get_name())
                    }
                }
            },
            TypedExpr::BoolIdentExpr(_, ident) => {
                let ident_value = self.var_env.get(&ident);
                match ident_value {
                    Some(Some(can_assign_obj)) => {
                        match can_assign_obj {
                            CanAssignObj::TypedExpr(TypedExpr::BoolExpr(..)) => {
                                ident.get_name()
                            },
                            _ => panic!("specified ident `{}` is not bool type. this type is {:?}", ident.get_name(), can_assign_obj)
                        }
                    }
                    _ => {
                        panic!("specified ident `{}` does not defined or initialized", ident.get_name())
                    }
                }
            },
            TypedExpr::NumBlockExpr(typed_ast_type, block) => self.block_to_ts(block, typed_ast_type),
            TypedExpr::BoolBlockExpr(typed_ast_type, block) => self.block_to_ts(block, typed_ast_type),
            TypedExpr::VoidBlockExpr(typed_ast_type, block) => self.block_to_ts(block, typed_ast_type),
            TypedExpr::NumAddExpr(_, l, r) => {
                format!("{}+{}", self.expr_to_ts(*l), self.expr_to_ts(*r))
            }
            TypedExpr::NumSubExpr(_, l, r) => {
                format!("{}-{}", self.expr_to_ts(*l), self.expr_to_ts(*r))
            }
            TypedExpr::NumMulExpr(_, l, r) => {
                format!("{}*{}", self.expr_to_ts(*l), self.expr_to_ts(*r))
            }
            TypedExpr::NumDivExpr(_, l, r) => {
                format!("{}/{}", self.expr_to_ts(*l), self.expr_to_ts(*r))
            },
            TypedExpr::NumLessThanOrEqualExpr(_, l, r) => {
                format!("{}<={}", self.expr_to_ts(*l), self.expr_to_ts(*r))
            },
            TypedExpr::CallExpr(_, call) => self.call_expr_to_ts(call),
        }
    }

    fn block_to_ts(&self, block: TypedBlock, typed_ast_type: TypedAstType) -> String {
        let block_var_env = self.var_env.clone();
        let ts_stmts = ToTs::to_ts(block.get_stmts(), Some(block_var_env));
        let ts_ast_type = self.typed_ast_type_to_ts(typed_ast_type);
        format!("():{}=>{{{}}}()", ts_ast_type, ts_stmts)
    }

    fn call_expr_to_ts(&self, typed_call_expr: TypedCallExpr) -> String {
        let func_name = typed_call_expr.get_func_name();
        if self.is_exist_ident(&func_name) {
            let ident_value = self.var_env.get(&func_name);
            match ident_value {
                Some(Some(can_assign_obj)) => match can_assign_obj {
                    CanAssignObj::TypedFunc(_) => {
                        let args = {
                            let args = typed_call_expr.get_args();
                            let mut str_args: String;
                            let first_arg = args.get(0);
                            let mut args_iter = args.iter();
                            args_iter.next();
                            match first_arg {
                                Some(arg) => {
                                    str_args = self.expr_to_ts(arg.clone());
                                    for arg in args_iter {
                                        str_args = format!(
                                            "{},{}",
                                            str_args,
                                            self.expr_to_ts(arg.clone())
                                        );
                                    }
                                    str_args
                                }
                                None => "".to_string(),
                            }
                        };
                        format!("{}({})", func_name.get_name(), args)
                    }
                    _ => panic!(
                        "specified variable `{}` does not func. this is {:?}",
                        func_name.get_name(),
                        can_assign_obj
                    ),
                },
                _ => panic!(
                    "specified func `{}` does not initialized",
                    func_name.get_name()
                ),
            }
        } else {
            panic!("specified func `{} does not defined", func_name.get_name());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::to_ts_rust::common_struct::CanAssignObj;
    use crate::to_ts_rust::to_ts::to_ts::ToTs;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedCallExpr, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent, TypedNumber, TypedReturnStmt, TypedStmt, TypedBool};
    use std::collections::HashMap;

    #[test]
    fn test_bool_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::BoolExpr(
            TypedAstType::Bool,
            TypedBool::new(true)
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "true;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumExpr(
            TypedAstType::Number,
            TypedNumber::new(0.0),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "0;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_ident_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumIdentExpr(
            TypedAstType::Number,
            TypedIdent::new("tmp1".to_string()),
        ))];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(
            TypedIdent::new("tmp1".to_string()),
            Some(CanAssignObj::TypedExpr(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(0.0),
            ))),
        );

        let ts_code = ToTs::to_ts(typed_stmts, Some(var_env));

        let expected_ts_code = "tmp1;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_bool_ident_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::BoolIdentExpr(
            TypedAstType::Bool,
            TypedIdent::new("tmp1".to_string()),
        ))];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(
            TypedIdent::new("tmp1".to_string()),
            Some(CanAssignObj::TypedExpr(TypedExpr::BoolExpr(
                TypedAstType::Bool,
                TypedBool::new(true)
            ))),
        );

        let ts_code = ToTs::to_ts(typed_stmts, Some(var_env));

        let expected_ts_code = "tmp1;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    #[should_panic]
    fn test_bool_ident_with_num_var_decl() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::BoolIdentExpr(
            TypedAstType::Bool,
            TypedIdent::new("tmp1".to_string()),
        ))];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(
            TypedIdent::new("tmp1".to_string()),
            Some(CanAssignObj::TypedExpr(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(10.0),
            ))),
        );

        ToTs::to_ts(typed_stmts, Some(var_env));
    }

    #[test]
    #[should_panic]
    fn test_num_ident_with_bool_var_decl() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumIdentExpr(
            TypedAstType::Number,
            TypedIdent::new("tmp1".to_string()),
        ))];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(
            TypedIdent::new("tmp1".to_string()),
            Some(CanAssignObj::TypedExpr(TypedExpr::BoolExpr(
                TypedAstType::Bool,
                TypedBool::new(true),
            ))),
        );

        ToTs::to_ts(typed_stmts, Some(var_env));
    }

    #[test]
    fn test_num_block_expr_stmt() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::num_block_new(
            vec![TypedStmt::return_new(TypedExpr::num_expr_new(1.0))]
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "():number=>{return 1;}();";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_bool_block_expr_stmt() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::bool_block_new(
            vec![TypedStmt::return_new(TypedExpr::bool_expr_new(true))]
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "():boolean=>{return true;}();";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_void_block_expr_stmt() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::void_block_new(vec![
            TypedStmt::expr_new(TypedExpr::num_expr_new(1.0))
        ]))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "():void=>{1;}();";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_multi_stmts_block_expr_stmt() {
        let typed_stmts = vec![TypedStmt::expr_new(TypedExpr::num_block_new(vec![
            TypedStmt::expr_new(TypedExpr::num_expr_new(1.0)),
            TypedStmt::expr_new(TypedExpr::bool_expr_new(true)),
            TypedStmt::return_new(TypedExpr::num_expr_new(2.0))
        ]))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "():number=>{1;true;return 2;}();";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_add_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumAddExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1.0),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "1+2;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_sub_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumSubExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1.0),
            )),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "2-1;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_mul_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumMulExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1.0),
            )),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "2*1;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_div_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumDivExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(4.0),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "4/2;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_less_than_or_equal_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumLessThanOrEqualExpr(
            TypedAstType::Bool,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(3.0),
            ))
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "2<=3;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_num_multi_op_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumAddExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumMulExpr(
                TypedAstType::Number,
                Box::new(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(2.0),
                )),
                Box::new(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(3.0),
                )),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2.0),
            )),
        ))];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "2*3+2;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_call_expr_stmt() {
        let typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::CallExpr(
            TypedAstType::Number,
            TypedCallExpr::new(
                TypedIdent::new("add".to_string()),
                vec![
                    TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1.0)),
                    TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2.0)),
                ],
            ),
        ))];

        let mut var_env: HashMap<TypedIdent, Option<CanAssignObj>> = HashMap::new();
        var_env.insert(
            TypedIdent::new("add".to_string()),
            Some(CanAssignObj::TypedFunc(TypedFunc::new(
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
            ))),
        );

        let ts_code = ToTs::to_ts(typed_stmts, Some(var_env));

        let expected_ts_code = "add(1,2);";

        assert_eq!(ts_code, expected_ts_code);
    }
}
