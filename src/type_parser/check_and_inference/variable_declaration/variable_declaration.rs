use crate::parser::ast::VariableDeclaration;
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::TypedVariableDeclaration;

impl TypeCheckAndInference {
    pub fn check_and_inference_var_declaration(
        &mut self,
        var_decl: VariableDeclaration,
    ) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());
        let type_name = match var_decl.get_type_name() {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag)),
            None => None,
        };
        let value = match var_decl.get_value() {
            Some(expr) => {
                let expr_value = self.check_and_inference_expr(expr);
                &self
                    .type_env
                    .insert(name.clone(), expr_value.get_typed_ast_type());
                Some(expr_value)
            }
            None => None,
        };

        TypedVariableDeclaration::new(name, type_name, value)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, Ident, Stmt, Types};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedExpr, TypedIdent, TypedNumber, TypedStmt, TypedVariableDeclaration, TypedBool};

    #[test]
    fn test_inference_var_declaration_number_type() {
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10.0, "10.0")),
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::NumberType),
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(10.0, "10.0".to_string()),
                )),
            ),
        )];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_var_declaration_bool_type() {
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::BoolType),
            Some(Expr::bool_new(true)),
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::BoolType),
                Some(TypedExpr::BoolExpr(
                    TypedAstType::Bool,
                    TypedBool::new(true)
                )),
            ),
        )];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


}
