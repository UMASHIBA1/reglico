mod test {
    use lalrpop_util::{lalrpop_mod};
    use super::super::ast;
    use crate::parser::ast::{Stmt, Ident, Types, Expr, Opcode, FuncArg, ReturnStmt};

    #[test]
    fn test_ident() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("tmp1;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(Expr::ident_new(Ident::new("tmp1".to_string())))
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("1 + 2;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Add,
                    Expr::num_new(2),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("2 - 1;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(2),
                    Opcode::Sub,
                    Expr::num_new(1),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_mul() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("2 * 2;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(2),
                    Opcode::Mul,
                    Expr::num_new(2),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_div() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("4 / 2;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(4),
                    Opcode::Div,
                    Expr::num_new(2),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add_and_sub() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("4 + 2 - 3;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::op_new(Expr::num_new(4), Opcode::Add, Expr::num_new(2)),
                    Opcode::Sub,
                    Expr::num_new(3),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub_and_add() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("4 - 2 + 3;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::op_new(Expr::num_new(4), Opcode::Sub, Expr::num_new(2)),
                    Opcode::Add,
                    Expr::num_new(3),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add_and_add() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("4 + 2 + 3;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::op_new(Expr::num_new(4), Opcode::Add, Expr::num_new(2)),
                    Opcode::Add,
                    Expr::num_new(3),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub_and_sub() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("6 - 2 - 3;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::op_new(Expr::num_new(6), Opcode::Sub, Expr::num_new(2)),
                    Opcode::Sub,
                    Expr::num_new(3),
                )
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_call() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("add(1,2);").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)],
                )
            ),
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_num() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("1;").unwrap();

        let expected_expr = vec![
            Stmt::expr_new(
                Expr::num_new(1)
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_number_type_is_not_stmt() {
        lalrpop_mod!(pub reglico);

        let result = reglico::ProgramParser::new().parse("number;").is_err();

        assert_eq!(result, true);
    }

    #[test]
    fn test_ident_with_no_semicolon() {
        lalrpop_mod!(pub reglico);

        let result = reglico::ProgramParser::new().parse("tmp1").is_err();

        assert_eq!(result, true);
    }

    #[test]
    fn test_call_with_no_semicolon() {
        lalrpop_mod!(pub reglico);

        let result = reglico::ProgramParser::new().parse("add(1,2)").is_err();

        assert_eq!(result, true);
    }

    #[test]
    fn test_op_with_no_semicolon() {
        lalrpop_mod!(pub reglico);

        let result = reglico::ProgramParser::new().parse("1 + 2").is_err();

        assert_eq!(result, true);
    }

    #[test]
    fn test_num_with_no_semicolon() {
        lalrpop_mod!(pub reglico);

        let result = reglico::ProgramParser::new().parse("1").is_err();

        assert_eq!(result, true);
    }

    #[test]
    fn test_const_assignment_with_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("const tmp1: number = 10;").unwrap();

        let expected_expr = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::NumberType),
                Some(Expr::num_new(10))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_const_assignment_no_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("const tmp1 = 10;").unwrap();

        let expected_expr = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                None,
                Some(Expr::num_new(10))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_return_stmt_is_stmt() {
        lalrpop_mod!(pub reglico);
        let result = reglico::ProgramParser::new().parse("return 1;").is_err();

        assert_eq!(result, false);

    }

    #[test]
    fn test_assign_ident_to_ident() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("const tmp1 = tmp2;").unwrap();

        let expected_expr = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                None,
                Some(Expr::ident_new(Ident::new("tmp2".to_string())))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_add_func() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("
            fn add(a: number, b: number) {
                return a + b;
            }
            const total = add(1, 2);
        ").unwrap();

        let expected_expr = vec![
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
                    )
                )],
            ),
            Stmt::var_new(
                Ident::new("total".to_string()),
                None,
                Some(Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)],
                ))
            ),
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_func_in_func() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("
            fn add(a: number, b: number) {
                fn add2(a: number, b: number) {
                    return a + b;
                }
                return add2(a, b);
            }
            const total = add(1, 2);
        ").unwrap();

        let expected_expr = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![
                    Stmt::func_new(
                        Ident::new("add2".to_string()),
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
                            )
                        )],
                    ),
                    Stmt::ReturnStmt(
                        ReturnStmt::new(
                            Expr::call_new(Ident::new("add2".to_string()), vec![
                                Expr::ident_new(Ident::new("a".to_string())),
                                Expr::ident_new(Ident::new("b".to_string())),
                            ])
                        )
                    )
                ],
            ),
            Stmt::var_new(
                Ident::new("total".to_string()),
                None,
                Some(Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)],
                ))
            ),
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_add_func_with_semicolon() {
        lalrpop_mod!(pub reglico);

        let result= reglico::ProgramParser::new().parse("
            fn add(a: number, b: number) {
                return a + b;
            };
        ").is_err();

        assert_eq!(result, true);
    }

}


