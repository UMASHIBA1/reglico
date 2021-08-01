mod test {
    use lalrpop_util::lalrpop_mod;
    use super::super::ast;


    #[test]
    fn test_const_assignment_with_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ConstAssignmentParser::new().parse("const tmp1: number = 10").unwrap();

        assert_eq!(&format!("{:?}", expr), "const tmp1: number = 10");

    }

    #[test]
    fn test_const_assignment_no_type() {
        lalrpop_mod!(pub reglico);

        let expr = reglico::ConstAssignmentParser::new().parse("const tmp1 = 10").unwrap();

        assert_eq!(&format!("{:?}", expr), "const tmp1 = 10");

    }

}

