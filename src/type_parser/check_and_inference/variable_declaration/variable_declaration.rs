use crate::parser::ast::VariableDeclaration;
use crate::type_parser::typed_ast::TypedVariableDeclaration;
use crate::type_parser::check_and_inference::_struct::TypeCheckAndInference;

impl TypeCheckAndInference {
    pub fn check_and_inference_var_declaration(&mut self, var_decl: VariableDeclaration) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());
        let type_name = match var_decl.get_type_name() {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag)),
            None => None
        };
        let value = match var_decl.get_value() {
            Some(expr) => {
                let expr_value = self.check_and_inference_expr(expr);
                &self.type_env.insert(name.clone(), expr_value.get_typed_ast_type());
                Some(expr_value)
            },
            None => None
        };

        TypedVariableDeclaration::new(
            name,
            type_name,
            value
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Ident, Types, Expr};
    use crate::type_parser::check_and_inference::check_and_inference::check_and_inference;
    use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedIdent, TypeFlag, TypedExpr, TypedAstType, TypedNumber};

    #[test]
    fn test_inference_var_declaration(){
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10))
        )];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new("tmp1".to_string()),
                    Some(TypeFlag::NumberType),
                    Some(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(10)))
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }
}