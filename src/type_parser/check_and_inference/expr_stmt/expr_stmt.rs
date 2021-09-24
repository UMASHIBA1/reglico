use crate::parser::ast::{CallExpr, Expr, Ident, Opcode, Operation, BlockExpr, Stmt};
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{TypedAstType, TypedCallExpr, TypedExpr, TypedNumber, TypedBool, TypedStmt, TypedBlock};

impl TypeCheckAndInference {
    pub fn check_and_inference_expr(&self, expr: Expr) -> TypedExpr {
        match expr {
            Expr::Num(num) => {
                TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(num.get_num()))
            },
            Expr::Bool(bool) => {
                TypedExpr::BoolExpr(TypedAstType::Bool, TypedBool::new(bool.get_bool()))
            },
            Expr::Op(operation) => self.check_and_inference_op(operation),
            Expr::Call(call_expr) => self.check_and_inference_call(call_expr),
            Expr::Ident(ident) => self.check_and_inference_ident(ident),
            Expr::Block(block) => self.check_and_inference_block(block),
        }
    }

    fn check_and_inference_block(&self, block: BlockExpr) -> TypedExpr {
        let stmts = block.get_stmts();

        let block_type_env = self.type_env.clone();

        let typed_stmts = TypeCheckAndInference::check_and_inference(stmts, Some(&block_type_env));

        let mut return_type: TypedAstType = TypedAstType::Void;
        for typed_stmt in &typed_stmts {
            match typed_stmt {
                TypedStmt::ReturnStmt(return_stmt) => {
                    return_type = return_stmt.get_return_type();
                    break;
                }
                _ => {}
            };
        };

        let typed_block = TypedBlock::new(typed_stmts);

        match return_type {
            TypedAstType::Number => {
                TypedExpr::NumBlockExpr(
                    TypedAstType::Number,
                    typed_block
                )
            },
            TypedAstType::Bool => {
                TypedExpr::BoolBlockExpr(
                    TypedAstType::Bool,
                    typed_block
                )
            },
            TypedAstType::Void => {
                TypedExpr::VoidBlockExpr(
                    TypedAstType::Void,
                    typed_block
                )
            }
            // TODO: Funcも返せるようにする
            _ => panic!("func does not allow to return: {:?}", block)
        }

    }

    fn check_and_inference_op(&self, operation: Operation) -> TypedExpr {
        match operation.get_operation() {
            // TODO: 後々String等の+演算等も出てくると思うのでここをStrAddExprとかFloatAddExprとか追加する
            (l, Opcode::Add, r) => TypedExpr::NumAddExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            ),
            (l, Opcode::Sub, r) => TypedExpr::NumSubExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            ),
            (l, Opcode::Mul, r) => TypedExpr::NumMulExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            ),
            (l, Opcode::Div, r) => TypedExpr::NumDivExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            ),
            (l, Opcode::LessThanOrEqual, r) => TypedExpr::NumLessThanOrEqualExpr(
                TypedAstType::Bool,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            )
        }
    }

    fn check_and_inference_call(&self, call_expr: CallExpr) -> TypedExpr {
        let func_name = call_expr.get_func_name();
        let args = call_expr.get_args();

        let typed_func_name = self.convert_ident_to_typed_ident(func_name);

        let typed_ast_type = self.type_env.get(&typed_func_name);

        let mut typed_args = vec![];
        for arg in args {
            typed_args.push(self.check_and_inference_expr(arg));
        }

        let func_return_ast_type = match typed_ast_type {
            Some(typed_ast_type) => match typed_ast_type {
                TypedAstType::Func(_, return_type) => match return_type {
                    Some(return_type) => return_type,
                    None => panic!("in type_parsing call, the calling func is not valid"),
                },
                _ => panic!("parsing call does not call func: {:?}", typed_ast_type),
            },
            None => panic!("parsing call does not defined value{:?}", &typed_func_name),
        };

        let typed_call_expr = TypedCallExpr::new(typed_func_name, typed_args);

        TypedExpr::CallExpr(*func_return_ast_type.clone(), typed_call_expr)
    }

    fn check_and_inference_ident(&self, ident: Ident) -> TypedExpr {
        let typed_ident = self.convert_ident_to_typed_ident(ident);
        let typed_ast_type = self.type_env.get(&typed_ident);
        match typed_ast_type {
            Some(typed_ast_type) => {
                match typed_ast_type {
                    TypedAstType::Number => {
                        TypedExpr::NumIdentExpr(typed_ast_type.clone(), typed_ident)
                    },
                    TypedAstType::Bool => {
                        TypedExpr::BoolIdentExpr(typed_ast_type.clone(), typed_ident)
                    },
                    // TODO: FuncIdentを加える
                    _ => panic!("parsing ident value's type does not allow `${:?}`, type:${:?} ", typed_ident, typed_ast_type.clone()),
                }
            }
            None => {
                panic!("parsing ident does not defined value${:?}", typed_ident);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, FuncArg, Ident, Opcode, ReturnStmt, Stmt, Types, BlockExpr};
    use crate::type_parser::type_parser::type_parser;
    use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedCallExpr, TypedExpr, TypedFunc, TypedFuncArg, TypedIdent, TypedNumber, TypedReturnStmt, TypedStmt, TypedVariableDeclaration, TypedBool};

    #[test]
    fn test_inference_num_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::num_new(10))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumExpr(
            TypedAstType::Number,
            TypedNumber::new(10)
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_bool_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::bool_new(false))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::BoolExpr(
            TypedAstType::Bool,
            TypedBool::new(false)
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_add_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1),
            Opcode::Add,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumAddExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_sub_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1),
            Opcode::Sub,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumSubExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_mul_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1),
            Opcode::Mul,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumMulExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_div_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1),
            Opcode::Div,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumDivExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_less_than_or_equal_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1),
            Opcode::LessThanOrEqual,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumLessThanOrEqualExpr(
            TypedAstType::Bool,
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(1),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_multi_op_expr_stmt() {
        let stmts = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(1), Opcode::Add, Expr::num_new(2)),
            Opcode::Mul,
            Expr::num_new(2),
        ))];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![TypedStmt::ExprStmt(TypedExpr::NumMulExpr(
            TypedAstType::Number,
            Box::new(TypedExpr::NumAddExpr(
                TypedAstType::Number,
                Box::new(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(1),
                )),
                Box::new(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(2),
                )),
            )),
            Box::new(TypedExpr::NumExpr(
                TypedAstType::Number,
                TypedNumber::new(2),
            )),
        ))];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_ident_expr_stmt() {
        let stmts = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::NumberType),
                Some(Expr::num_new(10)),
            ),
            Stmt::expr_new(Expr::ident_new(Ident::new("tmp1".to_string()))),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::NumberType),
                Some(TypedExpr::NumExpr(
                    TypedAstType::Number,
                    TypedNumber::new(10),
                )),
            )),
            TypedStmt::ExprStmt(TypedExpr::NumIdentExpr(
                TypedAstType::Number,
                TypedIdent::new("tmp1".to_string()),
            )),
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


    #[test]
    fn test_inference_bool_ident_expr_stmt() {
        let stmts = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::BoolType),
                Some(Expr::bool_new(true)),
            ),
            Stmt::expr_new(Expr::ident_new(Ident::new("tmp1".to_string()))),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(TypedVariableDeclaration::new(
                TypedIdent::new("tmp1".to_string()),
                Some(TypeFlag::BoolType),
                Some(TypedExpr::BoolExpr(
                    TypedAstType::Bool,
                    TypedBool::new(true)
                )),
            )),
            TypedStmt::ExprStmt(TypedExpr::BoolIdentExpr(
                TypedAstType::Bool,
                TypedIdent::new("tmp1".to_string()),
            )),
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_check_and_inference_num_block() {
        let stmts = vec![
            Stmt::expr_new(
            Expr::block_new(
                vec![
                    Stmt::return_new(Expr::num_new(1))
                    ]
                )
            )
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::expr_new(
                TypedExpr::num_block_new(
                    vec![
                        TypedStmt::return_new(
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))
                        )
                    ]
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_check_and_inference_bool_block() {
        let stmts = vec![
            Stmt::expr_new(
                Expr::block_new(
                    vec![
                        Stmt::return_new(Expr::bool_new(true))
                    ]
                )
            )
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::expr_new(
                TypedExpr::bool_block_new(
                    vec![
                        TypedStmt::return_new(
                            TypedExpr::BoolExpr(TypedAstType::Bool, TypedBool::new(true))
                        )
                    ]
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_check_and_inference_void_block() {
        let stmts = vec![
            Stmt::expr_new(
                Expr::block_new(
                    vec![
                        Stmt::expr_new(Expr::num_new(1))
                    ]
                )
            )
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::expr_new(
                TypedExpr::void_block_new(
                    vec![
                        TypedStmt::expr_new(
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))
                        )
                    ]
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_check_and_inference_multi_stmts_block() {
        let stmts = vec![
            Stmt::expr_new(
                Expr::block_new(
                    vec![
                        Stmt::expr_new(Expr::num_new(1)),
                        Stmt::expr_new(Expr::num_new(2)),
                        Stmt::return_new(Expr::bool_new(true))
                    ]
                )
            )
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::expr_new(
                TypedExpr::bool_block_new(
                    vec![
                        TypedStmt::expr_new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                        TypedStmt::expr_new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                        TypedStmt::return_new(
                            TypedExpr::BoolExpr(TypedAstType::Bool, TypedBool::new(true))
                        )
                    ]
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts);
    }

    #[test]
    fn test_inference_call() {
        let stmts = vec![
            Stmt::func_new(
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
            ),
            Stmt::expr_new(Expr::call_new(
                Ident::new("add".to_string()),
                vec![Expr::num_new(1), Expr::num_new(2)],
            )),
        ];

        let typed_stmts = type_parser(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(TypedFunc::new(
                TypedIdent::new("add".to_string()),
                vec![
                    TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                    TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                ],
                vec![],
                Some(TypedReturnStmt::new(TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumIdentExpr(
                        TypedAstType::Number,
                        TypedIdent::new("a".to_string()),
                    )),
                    Box::new(TypedExpr::NumIdentExpr(
                        TypedAstType::Number,
                        TypedIdent::new("b".to_string()),
                    )),
                ))),
            )),
            TypedStmt::ExprStmt(TypedExpr::CallExpr(
                TypedAstType::Number,
                TypedCallExpr::new(
                    TypedIdent::new("add".to_string()),
                    vec![
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                    ],
                ),
            )),
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }
}
