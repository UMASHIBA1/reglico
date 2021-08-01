mod test {
    use lalrpop_util::lalrpop_mod;
    use super::super::ast;


    #[test]
    fn calculator() {
        lalrpop_mod!(pub calculator);

        let expr = calculator::ExprParser::new()
            .parse("22 * 44 + 66")
            .unwrap();
        assert_eq!(&format!("{:?}", expr), "((22 * 44) + 66)");
    }

}

