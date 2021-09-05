use std::collections::HashMap;
use crate::type_parser::typed_ast::{TypedIdent, TypedStmt, TypedVariableDeclaration, TypeFlag, TypedExpr, TypedCallExpr};
use crate::to_js_rust::common_struct::CanAssignObj;

struct ToTs {
    var_env: HashMap<TypedIdent, Option<CanAssignObj>>
}

impl ToTs {
    pub fn to_ts(typed_stmts: Vec<TypedStmt>) -> String {
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


    fn type_flag_to_ts(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "number".to_string()
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