use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedIdent};
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

struct ToRust {
    var_env: HashMap<TypedIdent, Option<TypedExpr>>,
}

impl ToRust {

    pub fn to_rust(&self, typed_stmt: TypedStmt) -> String {
        match typed_stmt {
            TypedStmt::VariableDeclaration(var_decl) => self.var_decl_to_rust(var_decl),
            TypedStmt::ExprStmt(typed_expr) => self.expr_to_rust(typed_expr),
            _ => panic!("tmp panic"),
        }
    }

    fn var_decl_to_rust(&mut self, var_decl: TypedVariableDeclaration) -> String {
        let ident = var_decl.get_name();
        let name = ident.get_name();
        let type_name = var_decl.get_type_name();
        let value = var_decl.get_value();

        self.var_env.insert(ident, value.clone());

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

    fn type_flag_to_rust(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "i32".to_string()
        }
    }


    // TODO: 後でちゃんとしたもの作る
    fn expr_to_rust(&self, typed_expr: TypedExpr) -> String {
        match typed_expr {
            TypedExpr::NumExpr(_, num) =>  format!("{}", num.get_num()),
            TypedExpr::NumIdentExpr(_, ident) => {
                if self.is_exist_ident(&ident) {
                    ident.get_name()
                } else {
                    panic!("specified ident `{}` is not defined or initialized.", ident.get_name())
                }
            },
            TypedExpr::NumAddExpr(_, l, r) => format!("{} + {}", self.expr_to_rust(*l), self.expr_to_rust(*r)),
            _ => "".to_string() // TODO: call 作る
        }
        "10".to_string()
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

