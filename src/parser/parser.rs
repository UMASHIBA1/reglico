use crate::parser::ast::Stmt;
use lalrpop_util::lalrpop_mod;
use super::ast;

pub fn parser(reglico_code: &str) -> Vec<Stmt> {
    lalrpop_mod!(pub reglico);

    let will_stmts = reglico::ProgramParser::new().parse(reglico_code);

    match will_stmts {
        Ok(stmts) => stmts,
        Err(e) => panic!("parser error!: {}", e)
    }

}

mod test {
    #[allow(unused_imports)]
    use super::super::ast;
    #[allow(unused_imports)]
    use crate::parser::ast::{Expr, FuncArg, Ident, Opcode, ReturnStmt, Stmt, Types, BlockBox, CanElseStmt};
    #[allow(unused_imports)]
    use lalrpop_util::lalrpop_mod;
    #[allow(unused_imports)]
    use crate::parser::parser::parser;

    #[test]
    fn test_ident() {

        let expr = parser("tmp1;");

        let expected_expr = vec![Stmt::expr_new(Expr::ident_new(Ident::new(
            "tmp1".to_string(),
        )))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add() {

        let expr = parser("1 + 2;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1, "1"),
            Opcode::Add,
            Expr::num_new(2, "2"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub() {

        let expr = parser("2 - 1;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(2, "2"),
            Opcode::Sub,
            Expr::num_new(1, "1"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_mul() {

        let expr = parser("2 * 2;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(2, "2"),
            Opcode::Mul,
            Expr::num_new(2, "2"),
        ))];

        assert_eq!(expr, expected_expr);
    }


    #[test]
    fn test_op_less_than_or_equal() {

        let expr = parser("1 <= 2;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(1, "1"),
            Opcode::LessThanOrEqual,
            Expr::num_new(2, "2"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add_and_sub() {

        let expr = parser("4 + 2 - 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(4, "4"), Opcode::Add, Expr::num_new(2, "2")),
            Opcode::Sub,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub_and_add() {

        let expr = parser("4 - 2 + 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(4, "4"), Opcode::Sub, Expr::num_new(2, "2")),
            Opcode::Add,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add_and_add() {

        let expr = parser("4 + 2 + 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(4, "4"), Opcode::Add, Expr::num_new(2, "2")),
            Opcode::Add,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub_and_sub() {

        let expr = parser("6 - 2 - 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(6, "6"), Opcode::Sub, Expr::num_new(2, "2")),
            Opcode::Sub,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_add_and_mul() {

        let expr = parser("6 + 2 * 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(6, "6"),
            Opcode::Add,
            Expr::op_new(Expr::num_new(2, "2"), Opcode::Mul, Expr::num_new(3, "3")),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_mul_and_add() {

        let expr = parser("6 * 2 + 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(6, "6"), Opcode::Mul, Expr::num_new(2, "2")),
            Opcode::Add,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_sub_and_mul() {

        let expr = parser("8 - 2 * 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(8, "8"),
            Opcode::Sub,
            Expr::op_new(Expr::num_new(2, "2"), Opcode::Mul, Expr::num_new(3, "3")),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_mul_and_sub() {

        let expr = parser("6 * 2 - 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(6, "6"), Opcode::Mul, Expr::num_new(2, "2")),
            Opcode::Sub,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_mul_and_mul() {

        let expr = parser("8 * 2 * 3;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(8, "8"), Opcode::Mul, Expr::num_new(2, "2")),
            Opcode::Mul,
            Expr::num_new(3, "3"),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_if_else_block_stmt() {

        let expr = parser("if(true){1;}else{1;}");

        let expected_expr = vec![
            Stmt::if_stmt(
                Expr::bool_new(true),
                BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                Some(CanElseStmt::block_box_new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_if_elseif_stmt() {

        let expr = parser("if(true){1;}else if(true){1;}");

        let expected_expr = vec![
            Stmt::if_stmt(
                Expr::bool_new(true),
                BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                Some(CanElseStmt::if_stmt_new(
                    Expr::bool_new(true),
                    BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                    None
                ))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_if_stmt() {

        let expr = parser("if(true){1;}");

        let expected_expr = vec![
            Stmt::if_stmt(
                Expr::bool_new(true),
                BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                None
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_if_elseif_else_stmt() {

        let expr = parser("if(true){1;}else if(true){1;}else{1;}");

        let expected_expr = vec![
            Stmt::if_stmt(
                Expr::bool_new(true),
                BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                Some(CanElseStmt::if_stmt_new(
                    Expr::bool_new(true),
                    BlockBox::new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]),
                    Some(CanElseStmt::block_box_new(vec![Stmt::expr_new(Expr::num_new(1, "1"))]))
                ))
            )
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_less_than_or_equal_and_arithmetic() {

        let expr = parser("1 + 2 <= 2 * 2;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::op_new(Expr::num_new(1, "1"), Opcode::Add, Expr::num_new(2, "2")),
            Opcode::LessThanOrEqual,
            Expr::op_new(Expr::num_new(2, "2"), Opcode::Mul, Expr::num_new(2, "2")),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_less_than_or_equal_with_num_and_arithmetic() {

        let expr = parser("2 <= 2 * 2;");

        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            Expr::num_new(2, "2"),
            Opcode::LessThanOrEqual,
            Expr::op_new(Expr::num_new(2, "2"), Opcode::Mul, Expr::num_new(2, "2")),
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_arithmetic_with_call() {

        let expr = parser("add(1,2) + 3;");

        let add_call_expr= Expr::call_new(
            Ident::new("add".to_string()),
            vec![
                Expr::num_new(1, "1"),
                Expr::num_new(2, "2")
            ]
        );


        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            add_call_expr,
            Opcode::Add,
            Expr::num_new(3, "3")
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_op_less_than_or_equal_with_call() {

        let expr = parser("add(1,2) <= 4;");

        let add_call_expr= Expr::call_new(
            Ident::new("add".to_string()),
            vec![
                Expr::num_new(1, "1"),
                Expr::num_new(2, "2")
            ]
        );


        let expected_expr = vec![Stmt::expr_new(Expr::op_new(
            add_call_expr,
            Opcode::LessThanOrEqual,
            Expr::num_new(4, "4")
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_call() {

        let expr = parser("add(1,2);");

        let expected_expr = vec![Stmt::expr_new(Expr::call_new(
            Ident::new("add".to_string()),
            vec![Expr::num_new(1, "1"), Expr::num_new(2, "2")],
        ))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_block() {

        let expr = parser("{1;};");


        let expected_expr = vec![Stmt::expr_new(Expr::block_new(vec![
            Stmt::expr_new(Expr::num_new(1, "1"))
        ]))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_block_with_multi_stmts() {

        let expr = parser("{1;return 2;};");


        let expected_expr = vec![Stmt::expr_new(Expr::block_new(vec![
            Stmt::expr_new(Expr::num_new(1, "1")),
            Stmt::return_new(Expr::num_new(2, "2")),
        ]))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_num() {

        let expr = parser("1;");

        let expected_expr = vec![Stmt::expr_new(Expr::num_new(1, "1"))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_bool_true() {

        let expr = parser("true;");

        let expected_expr = vec![Stmt::expr_new(Expr::bool_new(true))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_bool_false() {

        let expr = parser("false;");

        let expected_expr = vec![Stmt::expr_new(Expr::bool_new(false))];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    #[should_panic]
    fn test_number_type_is_not_stmt() {

        parser("number;");

    }

    #[test]
    #[should_panic]
    fn test_ident_with_no_semicolon() {

        parser("tmp1");

    }

    #[test]
    #[should_panic]
    fn test_call_with_no_semicolon() {

        parser("add(1,2)");

    }

    #[test]
    #[should_panic]
    fn test_op_with_no_semicolon() {

        parser("1 + 2");

    }

    #[test]
    #[should_panic]
    fn test_num_with_no_semicolon() {

        parser("1");

    }

    #[test]
    fn test_const_assignment_with_type() {

        let expr = parser("const tmp1: number = 10;");

        let expected_expr = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10, "10")),
        )];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_const_assignment_no_type() {

        let expr = parser("const tmp1 = 10;");

        let expected_expr = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            None,
            Some(Expr::num_new(10, "10")),
        )];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_bool_type_declaration() {

        let expr = parser("const tmp1: bool = true;");

        let expected_expr = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::BoolType),
            Some(Expr::bool_new(true))
        )];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_return_stmt() {
        let expr = parser("return 1;");

        let expected_expr = vec![Stmt::return_new(
            Expr::num_new(1, "1"),
        )];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_assign_ident_to_ident() {

        let expr = parser("const tmp1 = tmp2;");

        let expected_expr = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            None,
            Some(Expr::ident_new(Ident::new("tmp2".to_string()))),
        )];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_add_func() {

        let expr = parser(
                "
            fn add(a: number, b: number) {
                return a + b;
            }
            const total = add(1, 2);
        ",
            );

        let expected_expr = vec![
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
            Stmt::var_new(
                Ident::new("total".to_string()),
                None,
                Some(Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1, "1"), Expr::num_new(2, "2")],
                )),
            ),
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_func_in_func() {

        let expr = parser(
                "
            fn add(a: number, b: number) {
                fn add2(a: number, b: number) {
                    return a + b;
                }
                return add2(a, b);
            }
            const total = add(1, 2);
        ",
            );

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
                        vec![Stmt::ReturnStmt(ReturnStmt::new(Expr::op_new(
                            Expr::ident_new(Ident::new("a".to_string())),
                            Opcode::Add,
                            Expr::ident_new(Ident::new("b".to_string())),
                        )))],
                    ),
                    Stmt::ReturnStmt(ReturnStmt::new(Expr::call_new(
                        Ident::new("add2".to_string()),
                        vec![
                            Expr::ident_new(Ident::new("a".to_string())),
                            Expr::ident_new(Ident::new("b".to_string())),
                        ],
                    ))),
                ],
            ),
            Stmt::var_new(
                Ident::new("total".to_string()),
                None,
                Some(Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1, "1"), Expr::num_new(2, "2")],
                )),
            ),
        ];

        assert_eq!(expr, expected_expr);
    }

    #[test]
    #[should_panic]
    fn test_add_func_with_semicolon() {

        parser(
                "
            fn add(a: number, b: number) {
                return a + b;
            };
        ",
            );

    }
}
