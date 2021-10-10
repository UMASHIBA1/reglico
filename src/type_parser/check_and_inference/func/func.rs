use crate::parser::ast::Func;
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{
    TypedAstType, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedStmt,
};

impl TypeCheckAndInference {
    pub fn check_and_inference_func(&mut self, func: Func) -> TypedFunc {
        let name = self.convert_ident_to_typed_ident(func.get_name());
        let args = func.get_func_args();
        let stmts = func.get_stmts();
        let mut arg_typed_ast_type = vec![];

        // NOTE: check_and_inference args type
        let mut typed_args = vec![];
        for arg in args {
            let arg_type = arg.get_arg_type();
            let typed_arg_type = self.convert_type_to_typed_type(arg_type.clone());

            arg_typed_ast_type.push(self.convert_type_to_typed_ast_type(arg_type));

            typed_args.push(TypedFuncArg::new(
                self.convert_ident_to_typed_ident(arg.get_name()),
                typed_arg_type,
            ));
        }

        let mut func_type_env = self.type_env.clone();
        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            func_type_env.insert(
                typed_arg.get_name(),
                arg_typed_ast_type.get(i).unwrap().clone(),
            );
        }

        let mut func_stmts =
            TypeCheckAndInference::check_and_inference(stmts, Some(&func_type_env));

        let mut return_typed_ast_type = TypedAstType::Void;
        for (_, stmt) in func_stmts.iter().enumerate() {
            match stmt {
                TypedStmt::ReturnStmt(return_stmt) => {
                    return_typed_ast_type = return_stmt.get_return_type();
                }
                // TODO: if_stmtの場合も追加
                _ => {}
            }
        }

        self.type_env.insert(
            name.clone(),
            TypedAstType::Func(arg_typed_ast_type, Box::new(return_typed_ast_type.clone())),
        );

        TypedFunc::new(name, typed_args, func_stmts, return_typed_ast_type)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, FuncArg, Ident, Opcode, ReturnStmt, Stmt, Types};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{
        TypeFlag, TypedAstType, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent, TypedReturnStmt,
        TypedStmt,
    };

    #[test]
    fn test_inference_func() {
        let stmts = vec![Stmt::func_new(
            Ident::new("add".to_string()),
            vec![
                FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
            ],
            vec![Stmt::ReturnStmt(ReturnStmt::new(Expr::op_new(
                Expr::ident_new(Ident::new("a".to_string())),
                Opcode::Add,
                Expr::ident_new(Ident::new("b".to_string())),
            )))],
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::Func(TypedFunc::new(
            TypedIdent::new("add".to_string()),
            vec![
                TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
            ],
            vec![TypedStmt::return_new(TypedExpr::NumAddExpr(
                TypedAstType::Number,
                Box::new(TypedExpr::NumIdentExpr(
                    TypedAstType::Number,
                    TypedIdent::new("a".to_string()),
                )),
                Box::new(TypedExpr::NumIdentExpr(
                    TypedAstType::Number,
                    TypedIdent::new("b".to_string()),
                )),
            ))],
            TypedAstType::Number
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }
}
