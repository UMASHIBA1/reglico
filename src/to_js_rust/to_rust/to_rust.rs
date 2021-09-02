use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedIdent, TypedFunc, TypedReturnStmt, TypedCallExpr};
use std::collections::HashMap;

// NOTE: add関数をRustに変換すると
// ```reglico
//  fn add(a: number, b: number) {
//      return a + b;
//  }
//
//  const total = add(1,2);
//  ```
//  ↓
//  ```rust
//  fn add(a: number, b: number) {
//      a + b
//  }
//  let total = add(1,2);

#[derive(Debug)]
enum CanAssignObj {
    TypedFunc(TypedFunc),
    TypedExpr(TypedExpr),
}

struct ToRust {
    var_env: HashMap<TypedIdent, Option<CanAssignObj>>,
}

impl ToRust {

    pub fn to_rust(&mut self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_rust(var_decl),
            TypedStmt::ExprStmt(typed_expr) => self.expr_to_rust(typed_expr),
            TypedStmt::Func(typed_func) =>  self.func_to_rust(typed_func),
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
                    Some(type_flag) => format!("let {}: {} = {};", name, self.type_flag_to_rust(type_flag), self.expr_to_rust(typed_expr)),
                    None => format!("let {} = {};", name, self.expr_to_rust(typed_expr)),
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
            TypedExpr::NumAddExpr(_, l, r) => format!("{} + {}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            TypedExpr::CallExpr(_, call) => self.call_expr_to_rust(call)
        }
        "10".to_string()
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
                                CanAssignObj::TypedFunc(typed_func) => {
                                    let args = {
                                        let args = typed_call_expr.get_args();
                                        let mut str_args: String;

                                        let first_arg = args.get(0);
                                        args.iter().next();
                                        match first_arg {
                                            Some(arg) => {
                                                str_args = self.expr_to_rust(arg.clone());
                                                for arg in args {
                                                    str_args = format!("{}, {}", str_args, self.expr_to_rust(arg.clone()));
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
            name,
            Some(CanAssignObj::TypedFunc(typed_func))
        );

        let args_str = {
            let mut args_str: String;
            let first_arg = args.get(0);
            match first_arg {
                Some(first_arg) => {
                    args_str = format!("{}: {}", first_arg.get_name().get_name(), self.type_flag_to_rust(first_arg.get_arg_type()));
                    args.iter().next();

                    for typed_func_arg in args {
                        let arg = format!("{}: {}",
                                          typed_func_arg.get_name().get_name(),
                                          self.type_flag_to_rust(typed_func_arg.get_arg_type()));
                        args_str = format!("{}, {}", args_str, arg);
                    }
                },
                None => {
                    args_str = "".to_string();
                }
            };
            args_str
        };

        let stmts_str = {
            let mut stmts_str = "".to_string();
            for stmt in stmts {
                stmts_str = format!("{}\n{}", stmts_str, self.to_rust(stmt));
            }
            stmts_str
        };

        match &return_stmt {
            Some(typed_return_stmt) => {
                format!("
                fn {} ({}){{
                    {}
                }}
                {}
                ", name.get_name(), args_str, stmts_str, self.return_stmt_to_rust(typed_return_stmt)
                )
            },
            None => {
                format!("
                fn {} ({}){{
                    {}
                }}
                ", name.get_name(), args_str, stmts_str
                )
            },
        }
    }

    fn return_stmt_to_rust(&self, return_stmt: &TypedReturnStmt) -> String {
        self.expr_to_rust(return_stmt.get_expr())
    }

    fn type_flag_to_rust(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "i32".to_string()
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

