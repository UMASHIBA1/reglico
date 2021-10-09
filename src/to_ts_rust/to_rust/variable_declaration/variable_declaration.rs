use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::type_parser::typed_ast::TypedVariableDeclaration;

impl ToRust {
    pub fn var_decl_to_rust(&mut self, var_decl: TypedVariableDeclaration) -> String {
        let ident = var_decl.get_name();
        let name = ident.get_name();
        let type_name = var_decl.get_type_name();
        let value = var_decl.get_value();

        match &value {
            Some(expr) => self
                .var_env
                .insert(ident, Some(CanAssignObj::TypedExpr(expr.clone()))),
            None => self.var_env.insert(ident, None),
        };

        // TODO: 所有権について後でどうするかちゃんと考える
        match value {
            Some(typed_expr) => match type_name {
                Some(type_flag) => format!(
                    "let {}:{}={};",
                    name,
                    self.type_flag_to_rust(type_flag),
                    self.expr_to_rust(typed_expr)
                ),
                None => format!("let {}={};", name, self.expr_to_rust(typed_expr)),
            },
            None => format!("let mut {};", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::to_ts_rust::to_rust::to_rust::ToRust;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedExpr, TypedIdent, TypedNumber, TypedStmt, TypedVariableDeclaration, TypedBool};

    #[test]
    fn test_no_type_var_declaration() {
        let typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                None,
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(0.0, "0.0".to_string()),
                )),
            ),
        )];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "let tmp1=0;";

        assert_eq!(rust_code, expected_rust_code)
    }

    #[test]
    fn test_has_type_number_var_declaration() {
        let typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::NumberType),
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(0.0, "0.0".to_string()),
                )),
            ),
        )];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "let tmp1:f32=0;";

        assert_eq!(rust_code, expected_rust_code);
    }

    #[test]
    fn test_has_type_bool_var_declaration() {
        let typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::BoolType),
                Some(TypedExpr::BoolExpr(
                    TypedAstType::Bool,
                    TypedBool::new(true),
                )),
            ),
        )];

        let rust_code = ToRust::to_rust(typed_stmts, None);

        let expected_rust_code = "let tmp1:bool=true;";

        assert_eq!(rust_code, expected_rust_code);
    }

}
