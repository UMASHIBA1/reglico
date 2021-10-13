use crate::parser::ast::{Func, Stmt};
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{
    TypedAstType, TypedFunc, TypedFuncArg, TypedStmt,
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
        let mut arg_ast_types = vec![];
        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            let arg_type = arg_typed_ast_type.get(i).unwrap().clone();
            arg_ast_types.push(arg_type.clone());
            func_type_env.insert(
                typed_arg.get_name(),
                arg_type,
            );
        }

        func_type_env.insert(
            name.clone(),
            TypedAstType::Func(
                arg_ast_types,
                Box::new(TypedAstType::LazyEval),
            )
        );
        
        // NOTE: get return_type_ast_type before inference for recursive func.
        let return_typed_ast_type = {
            let mut can_have_return_type_stmts = vec![];
            for stmt in &stmts {
                match stmt {
                    Stmt::IfStmt(_) | Stmt::ReturnStmt(_) => {
                        can_have_return_type_stmts.push(stmt.clone());
                    },
                    _ => {}
                }
            }

            let can_have_return_type_typed_stmts = TypeCheckAndInference::check_and_inference(can_have_return_type_stmts, Some(&func_type_env));

            let mut this_func_return_type = TypedAstType::Void;
            for stmt in can_have_return_type_typed_stmts {
                match stmt {
                    TypedStmt::IfStmt(if_stmt) => {
                        let return_type = if_stmt.get_return_ast_type();
                        if (return_type != TypedAstType::Void) | (return_type != TypedAstType::LazyEval) {
                            this_func_return_type = return_type;
                        }
                    },
                    TypedStmt::ReturnStmt(return_stmt) => {
                        let return_type = return_stmt.get_return_type();
                        if (return_type != TypedAstType::Void) | (return_type != TypedAstType::LazyEval) {
                            this_func_return_type = return_type;
                        }
                    }
                    _ => {}
                }
            }

            this_func_return_type

        };

        func_type_env.insert(
            name.clone(),
            TypedAstType::Func(arg_typed_ast_type.clone(), Box::new(return_typed_ast_type.clone())),
        );

        self.type_env.insert(
            name.clone(),
            TypedAstType::Func(arg_typed_ast_type, Box::new(return_typed_ast_type.clone())),
        );

        let mut func_stmts =
            TypeCheckAndInference::check_and_inference(stmts, Some(&func_type_env));

        TypedFunc::new(name, typed_args, func_stmts, return_typed_ast_type)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, FuncArg, Ident, Opcode, ReturnStmt, Stmt, Types, BlockBox, ExprStmt};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent, TypedStmt, TypedBlockBox};

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

    #[test]
    fn test_inference_func_with_return_stmt_included_in_if_stmt() {
        // fn tmp() {if(true){return 1;}}
        let stmts = vec![Stmt::func_new(
            Ident::new("tmp".to_string()),
            vec![],
            vec![
                Stmt::if_stmt(
                    Expr::bool_new(true),
                    BlockBox::new(vec![Stmt::return_new(Expr::num_new(1, "1"))]),
                    None
                )
            ],
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::Func(TypedFunc::new(
            TypedIdent::new("tmp".to_string()),
            vec![],
            vec![TypedStmt::if_stmt_new(
                TypedExpr::bool_expr_new(true),
                TypedBlockBox::new(
                    vec![TypedStmt::return_new(TypedExpr::num_expr_new(1, "1".to_string()))],
                    TypedAstType::Number
                ),
                None,
                TypedAstType::Number
            )],
            TypedAstType::Number
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_recursive_func() {
        // fn tmp(a: number) {tmp(a - 1);}
        let stmts = vec![Stmt::func_new(
            Ident::new("tmp".to_string()),
            vec![
                FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
            ],
            vec![
                Stmt::ExprStmt(ExprStmt::new(
                    Expr::call_new(
                        Ident::new("tmp".to_string()),
                        vec![
                            Expr::op_new(
                                Expr::ident_new(Ident::new("a".to_string())),
                                Opcode::Sub,
                                Expr::num_new(1, "1")
                            )
                        ]
                    )
                ))
            ],
        )];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::Func(TypedFunc::new(
            TypedIdent::new("tmp".to_string()),
            vec![
                TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType)
            ],
            vec![
                TypedStmt::expr_new(
                    TypedExpr::call_expr_new(
                        TypedAstType::Void,
                        TypedIdent::new("tmp".to_string()),
                        vec![
                            TypedExpr::num_sub_new(
                                TypedExpr::num_ident_new(TypedIdent::new("a".to_string())),
                                TypedExpr::num_expr_new(1, "1".to_string()),
                            ),
                        ]
                    )
                )
            ],
            TypedAstType::Void
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


}
