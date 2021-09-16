use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::type_parser::typed_ast::TypedVariableDeclaration;

impl ToTs {
    pub fn var_decl_to_ts(&mut self, var_decl: TypedVariableDeclaration) -> String {
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

        match value {
            Some(typed_expr) => match type_name {
                Some(type_flag) => format!(
                    "const {}:{}={};",
                    name,
                    self.type_flag_to_ts(type_flag),
                    self.expr_to_ts(typed_expr)
                ),
                None => format!("const {}={};", name, self.expr_to_ts(typed_expr)),
            },
            None => format!("let {};", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::to_ts_rust::to_ts::to_ts::ToTs;
    use crate::type_parser::typed_ast::{
        TypeFlag, TypedAstType, TypedExpr, TypedIdent, TypedNumber, TypedStmt,
        TypedVariableDeclaration,
    };

    #[test]
    fn test_no_type_var_declaration() {
        let typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                None,
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(0),
                )),
            ),
        )];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "const tmp1=0;";

        assert_eq!(ts_code, expected_ts_code);
    }

    #[test]
    fn test_has_type_var_declaration() {
        let typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::NumberType),
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(0),
                )),
            ),
        )];

        let ts_code = ToTs::to_ts(typed_stmts, None);

        let expected_ts_code = "const tmp1:number=0;";

        assert_eq!(ts_code, expected_ts_code);
    }
}
