use crate::parser::ast::VariableDeclaration;
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::TypedVariableDeclaration;

impl TypeCheckAndInference {
    pub fn check_and_inference_var_declaration(
        &mut self,
        var_decl: VariableDeclaration,
    ) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());

        let type_name = var_decl.get_type_name();

        let typed_type_name = match &type_name {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag.clone())),
            None => None,
        };

        let value;
        match var_decl.get_value() {
            Some(expr) => {
                let expr_value = self.check_and_inference_expr(expr);

                let expr_typed_ast_type = expr_value.get_typed_ast_type();

                match type_name {
                    Some(types) => {
                        // type check for var_decl
                        let expected_typed_ast_type = self.convert_type_to_typed_ast_type(types);

                        if expr_typed_ast_type != expected_typed_ast_type {
                            panic!("{:?}'s variable declaration: {:?} can not assign to {:?}", name.get_name(), expr_typed_ast_type, expected_typed_ast_type);
                        }
                    },
                    _ => {}
                };


                self
                    .type_env
                    .insert(name.clone(), expr_typed_ast_type);
                value = Some(expr_value);
            }
            None => {value = None},
        };

        TypedVariableDeclaration::new(name, typed_type_name, value)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, Ident, Stmt, Types};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedExpr, TypedIdent, TypedNumber, TypedStmt, TypedVariableDeclaration, TypedBool};

    #[test]
    #[should_panic]
    fn test_check_num_var_declaration_assigned_bool() {
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::bool_new(true)),
        )];

        type_parser(stmts);
    }

    #[test]
    fn test_inference_var_declaration_number_type() {
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10, "10")),
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::NumberType),
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(10, "10".to_string()),
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
