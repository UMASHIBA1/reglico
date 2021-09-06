use std::collections::HashMap;
use crate::type_parser::typed_ast::{TypedIdent, TypedStmt, TypedVariableDeclaration, TypeFlag, TypedExpr, TypedCallExpr, TypedFunc, TypedNumber, TypedAstType};
use crate::to_js_rust::common_struct::CanAssignObj;

struct ToTs {
    var_env: HashMap<TypedIdent, Option<CanAssignObj>>
}

impl ToTs {
    pub fn to_ts(typed_stmts: Vec<TypedStmt>, var_env: Option<&HashMap<TypedIdent, Option<CanAssignObj>>>) -> String {
        let mut js_code = "".to_string();

        let mut to_js = ToTs::new();
        for typed_stmt in typed_stmts {
            js_code = format!("{}{}", js_code, to_js.stmt_to_ts(typed_stmt));
        };
        js_code
    }

    fn new() -> ToTs {
        ToTs {
            var_env: HashMap::new(),
        }
    }

    fn stmt_to_ts(&mut self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_ts(var_decl),
            TypedStmt::ExprStmt(typed_expr) => self.expr_to_ts(typed_expr),
            TypedStmt::Func(typed_func) => self.func_to_ts(typed_func),
            _ => "".to_string() // TODO: 後で他のstmtも作る
        }
    }

    fn var_decl_to_ts(&mut self, var_decl: TypedVariableDeclaration) -> String {
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

        match value {
            Some(typed_expr) => {
                match type_name {
                    Some(type_flag) => format!("const {}:{}={};", name, self.type_flag_to_ts(type_flag), self.expr_to_ts(typed_expr)),
                    None => format!("let {}={};", name, self.expr_to_ts(typed_expr)),
                }
            },
            None => format!("let mut {};", name),
        }
    }

    fn expr_to_ts(&self, typed_expr: TypedExpr) -> String {
        match typed_expr {
            TypedExpr::NumExpr(_, num) => format!("{}", num.get_num()),
            TypedExpr::NumIdentExpr(_, ident) => {
                if self.is_exist_ident(&ident) {
                    ident.get_name()
                } else {
                    panic!("specified ident `{}` does not defined or initialized", ident.get_name())
                }
            },
            TypedExpr::NumAddExpr(_, l, r) => format!("{}+{}", self.expr_to_ts(*l), self.expr_to_ts(*r)),
            _ => "".to_string() // TODO: 後でCall作る

        }
    }

    fn func_to_ts(&mut self, typed_func: TypedFunc) -> String {
        let name = typed_func.get_name();
        let args = typed_func.get_args();
        let stmts = typed_func.get_stmts();
        let return_stmt = typed_func.get_return_stmt();

        self.var_env.insert(
            name.clone(),
            Some(CanAssignObj::TypedFunc(typed_func))
        );

        let mut func_var_env = self.var_env.clone();
        for arg in args {
            func_var_env.insert(
                arg.get_name(),
                Some(
                    CanAssignObj::TypedExpr(
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0))
                    )
                )
            )
        };

        let args_str = {
            let mut args_str: String;
            let first_arg = &args.get(0);
            match first_arg {
                Some(first_arg) => {
                    args_str = format!("{}:{}", first_arg.get_name().get_name(), self.type_flag_to_ts(first_arg.get_arg_type()));
                    let mut args_iter = args.iter();
                    args_iter.next();

                    for typed_func_arg in args_iter {
                        let arg = format!("{}:{}",
                                          typed_func_arg.get_name().get_name(),
                                          self.type_flag_to_ts(typed_func_arg.get_arg_type())
                        );
                        args_str = format!("{},{}", args_str, arg);
                    }
                },
                None => {
                    args_str = "".to_string();
                }
            }
            args_str
        };

        let stmts_str = ToTs::to_ts(stmts, Some(&func_var_env));


        let (return_type, return_stmt_str) = {
            match &return_stmt {
                Some(typed_return_stmt) => {
                    let return_type = self.typed_ast_type_to_ts(typed_return_stmt.get_return_type());
                    let return_stmt_str = ToTs::to_ts(vec![TypedStmt::ReturnStmt(typed_return_stmt.clone())], Some(&func_var_env));
                    (return_type, Some(return_stmt_str))
                },
                None => ("void".to_string(), None)
            }
        };

        match return_stmt_str {
            Some(return_stmt_str) => {
                format!("const {}({}):{}{{{}{}}}", name.get_name(), args_str, return_type, stmts_str, return_stmt_str)
            },
            None => {
                format!("const {}({}):void{{{}}}", name.get_name(), args_str, stmts_str)
            }
        }
    }


    fn type_flag_to_ts(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "number".to_string()
        }
    }

    fn typed_ast_type_to_ts(&self, typed_ast_type: TypedAstType) -> String {
        match typed_ast_type {
            TypedAstType::Number => "number".to_string(),
            TypedAstType::Void => "void",
            _ => "void".to_string() // TODO: Funcの型生成する
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