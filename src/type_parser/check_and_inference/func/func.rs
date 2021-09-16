use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::parser::ast::Func;
use crate::type_parser::typed_ast::{TypedFunc, TypedFuncArg, TypedReturnStmt, TypedStmt, TypedAstType};

impl TypeCheckAndInference {
    pub fn check_and_inference_func(&mut self, func: Func) -> TypedFunc {
        let name = self.convert_ident_to_typed_ident(func.get_name());
        let args = func.get_func_args();
        let stmts =  func.get_stmts();
        let mut arg_typed_ast_type = vec![];

        // NOTE: check_and_inference args type
        let mut typed_args = vec![];
        for arg in args {
            let arg_type = arg.get_arg_type();
            let typed_arg_type = self.convert_type_to_typed_type(arg_type.clone());

            arg_typed_ast_type.push(
                self.convert_type_to_typed_ast_type(arg_type)
            );

            typed_args.push(
                TypedFuncArg::new(
                    self.convert_ident_to_typed_ident(arg.get_name()),
                    typed_arg_type,
                )
            );
        };

        let mut func_type_env = self.type_env.clone();
        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            func_type_env.insert(typed_arg.get_name(), arg_typed_ast_type.get(i).unwrap().clone());
        };

        let mut func_stmts = TypeCheckAndInference::check_and_inference(stmts, Some(&func_type_env));
        let mut typed_return_stmt: Option<TypedReturnStmt> = None;
        let mut return_stmt_index: Option<usize> = None;
        for (i, stmt) in func_stmts.iter().enumerate() {
            match stmt {
                TypedStmt::ReturnStmt(_) => {
                    return_stmt_index = Some(i);
                },
                _ => {}
            }
        }
        match return_stmt_index {
            Some(i) => {
                let will_typed_return_stmt = func_stmts.remove(i);
                match will_typed_return_stmt {
                    TypedStmt::ReturnStmt(return_stmt) => {
                        typed_return_stmt = Some(return_stmt);
                    }
                    _ => {},
                };
            },
            _ => {}
        }

        let return_typed_ast_type = {
            match &typed_return_stmt {
                Some(typed_return_stmt) => {
                    Some(Box::new(typed_return_stmt.get_expr().get_typed_ast_type()))
                },
                None => Some(Box::new(TypedAstType::Void)),
            }
        };

        self.type_env.insert(name.clone(), TypedAstType::Func(arg_typed_ast_type, return_typed_ast_type));

        TypedFunc::new(
            name,
            typed_args,
            func_stmts,
            typed_return_stmt
        )

    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Ident, FuncArg, Types, ReturnStmt, Expr, Opcode};
    use crate::type_parser::typed_ast::{TypedStmt, TypedFunc, TypedIdent, TypedFuncArg, TypeFlag, TypedReturnStmt, TypedExpr, TypedAstType};
    use crate::type_parser::type_parser::type_parser;

    #[test]
    fn test_inference_func(){
        let stmts = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![Stmt::ReturnStmt(
                    ReturnStmt::new(
                        Expr::op_new(
                            Expr::ident_new(Ident::new("a".to_string())),
                            Opcode::Add,
                            Expr::ident_new(Ident::new("b".to_string())),
                        )
                    ))
                ],
            ),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),

                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }
}