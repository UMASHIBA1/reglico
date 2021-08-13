mod test {
    use lalrpop_util::lalrpop_mod;
    use super::super::ast;
    use crate::parser::ast::{Stmt, VariableDeclaration, Ident, Types, Expr, ExprStmt, Opcode};


    #[test]
    fn test_const_assignment_with_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("const tmp1: number = 10;").unwrap();

        let will_expr = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::NumberType),
                Some(Expr::num_new(10))
            )
        ];

        assert_eq!(expr, will_expr);

    }

    #[test]
    fn test_const_assignment_no_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("const tmp1 = 10;").unwrap();

        let will_expr = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                None,
                Some(Expr::num_new(10))
            )
        ];

        assert_eq!(expr, will_expr);
    }

    #[test]
    fn test_add() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("1 + 2;").unwrap();

        let will_expr = vec![
            Stmt::ExprStmt(
                ExprStmt {
                    expr: Expr::op_new(
                        Expr::num_new(1),
                        Opcode::Add,
                        Expr::num_new(2),
                    )
                }
            )
        ];

        assert_eq!(expr, will_expr);
    }

    #[test]
    fn test_function() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ProgramParser::new().parse("fn add(a: number, b: number) {
        const tmp1 = 32;
        1 + 2;
        3;
        fn func1(){
            const tmp2 = 1;
            2;
        }
        const tmp2 = tmp1;
        return 1 + 2;
        }
        add(1,2);
        ").unwrap();

        assert_eq!(&format!("{:?}", expr), "tmp");
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

        assert_eq!(&format!("{:?}", expr), "tmp");
    }

}
