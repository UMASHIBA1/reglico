use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedIdent, TypedFunc, TypedReturnStmt, TypedCallExpr, TypedAstType, TypedNumber};
use std::collections::HashMap;
use crate::to_js_rust::common_struct::CanAssignObj;

struct ToRust {
    var_env: HashMap<TypedIdent, Option<CanAssignObj>>,
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

    fn var_decl_to_rust(&mut self, var_decl: TypedVariableDeclaration) -> String {
        let ident = var_decl.get_name();
        let name = ident.get_name();
        let type_name = var_decl.get_type_name();
        let value = var_decl.get_value();

        match &value {
            Some(expr) => {
              self.var_env.insert(ident, Some(CanAssignObj::TypedExpr(expr.clone())))
            },
            None => {self.var_env.insert(ident, None)},
        };


        // TODO: 所有権について後でどうするかちゃんと考える
        match value {
            Some(typed_expr) => {
                match type_name {
                    Some(type_flag) => format!("let {}:{}={};", name, self.type_flag_to_rust(type_flag), self.expr_to_rust(typed_expr)),
                    None => format!("let {}={};", name, self.expr_to_rust(typed_expr)),
                }
            },
            None => format!("let mut {};", name),
        }
    }


    fn expr_to_rust(&self, typed_expr: TypedExpr) -> String {
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


    fn func_to_rust(&mut self, typed_func: TypedFunc) -> String {
        let name = typed_func.get_name();
        let args = typed_func.get_args();
        let stmts = typed_func.get_stmts();
        let return_stmt = typed_func.get_return_stmt();
        self.var_env.insert(
            name.clone(),
            Some(CanAssignObj::TypedFunc(typed_func))
        );

        let mut func_var_env = self.var_env.clone();

        for arg in &args {
            func_var_env.insert(
                arg.get_name(),
                Some(
                    CanAssignObj::TypedExpr(
                        // NOTE: expr_to_rust関数の変数参照を騙すために0を入れてます
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0))
                    )
                )
            );
        }

        let args_str = {
            let mut args_str: String;
            let first_arg = &args.get(0);
            match first_arg {
                Some(first_arg) => {
                    args_str = format!("{}:{}", first_arg.get_name().get_name(), self.type_flag_to_rust(first_arg.get_arg_type()));
                    let mut args_iter = args.iter();
                    args_iter.next();

                    for typed_func_arg in args_iter {
                        let arg = format!("{}:{}",
                                          typed_func_arg.get_name().get_name(),
                                          self.type_flag_to_rust(typed_func_arg.get_arg_type()));
                        args_str = format!("{},{}", args_str, arg);
                    }
                },
                None => {
                    args_str = "".to_string();
                }
            };
            args_str
        };

        let stmts_str = ToRust::to_rust(stmts, Some(func_var_env.clone()));
        
        let (return_type, return_stmt_str) = {
            match &return_stmt {
                Some(typed_return_stmt) => {
                    let return_type = self.typed_ast_type_to_rust(typed_return_stmt.get_return_type());
                    let return_stmt_str = ToRust::to_rust(vec![TypedStmt::ReturnStmt(typed_return_stmt.clone())], Some(func_var_env));
                    (return_type, Some(return_stmt_str))
                },
                None => ("()".to_string(), None)
            }
        };

        match return_stmt_str {
            Some(return_stmt_str) => {
                format!("fn {}({})->{}{{{}{}}}", name.get_name(), args_str, return_type, stmts_str, return_stmt_str
                )
            },
            None => {
                format!("fn {}({})->(){{{}}}", name.get_name(), args_str, stmts_str)
            },
        }
    }

    fn return_stmt_to_rust(&self, return_stmt: &TypedReturnStmt) -> String {
        let expr = return_stmt.get_expr();
            self.expr_to_rust(expr)
    }

    fn type_flag_to_rust(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "i32".to_string()
        }
    }

    fn typed_ast_type_to_rust(&self, typed_ast_type: TypedAstType) -> String {
        match typed_ast_type {
            TypedAstType::Number => "i32".to_string(),
            TypedAstType::Void => "()".to_string(),
            _ => "()".to_string() // TODO: Funcの型生成するの面倒くさいので後回しにしてます、あとでやりましょう！
        }
    }

    fn is_exist_ident(&self, ident: &TypedIdent) -> bool {
        match self.var_env.get(ident) {
            Some(option_type_expr) => {
                match option_type_expr {
                    Some(_) => true,
                    None => false,
                }
            },
            None => false,
        }
    }

}



#[cfg(test)]
mod tests {
    use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedFunc, TypedFuncArg, TypeFlag, TypedReturnStmt, TypedAstType, TypedCallExpr, TypedNumber, TypedExpr, TypedVariableDeclaration};
    use crate::to_js_rust::to_rust::to_rust::ToRust;
    use std::collections::HashMap;
    use crate::to_js_rust::common_struct::CanAssignObj;

    #[test]
    fn test_no_type_var_declaration() {
        let typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new(
                        "tmp1".to_string()),
                    None,
                    Some(
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0))
                    ))
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "let tmp1=0;";

        assert_eq!(rust_code, expected_rust_code)
    }

    #[test]
    fn test_has_type_var_declaration() {
        let typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new(
                        "tmp1".to_string()),
                    Some(TypeFlag::NumberType),
                    Some(
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0))
                    ))
            )
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "let tmp1:i32=0;";

        assert_eq!(rust_code, expected_rust_code);
    }

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


    #[test]
    fn test_func_declaration() {
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
        ];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "fn add(a:i32,b:i32)->i32{a+b}";

        assert_eq!(rust_code, expected_rust_code);
    }

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