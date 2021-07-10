use super::tokens::Token;
pub fn lexer(code: &str) -> Vec<Token> {
    vec![]
}

#[cfg(test)]
mod tests {
    use super::lexer;
    use super::super::tokens::DebugInfo;
    use crate::lexer::tokens::Token::{COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, PLUS, ASSIGN, NUMBER, CONST, FUNC, IDENT, NUMBER_TYPE, COLON, RETURN};

    fn create_sample_debug_info() -> DebugInfo {
        DebugInfo::new("for_test.reg", 1)
    }

    #[test]
    fn tokenize_delimiter() {
        let sample_debug_info = create_sample_debug_info();

        let input = ",;(){}";
        let expected = vec![
            COMMA(sample_debug_info.clone()),
            SEMICOLON(sample_debug_info.clone()),
            LPAREN(sample_debug_info.clone()),
            RPAREN(sample_debug_info.clone()),
            LBRACE(sample_debug_info.clone()),
            RBRACE(sample_debug_info),
        ];

        let output = lexer(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_operator() {
        let sample_debug_info = create_sample_debug_info();

        let input = "+=";
        let expected = vec![
            PLUS(sample_debug_info.clone()),
            ASSIGN(sample_debug_info)
        ];

        let output = lexer(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_types() {
        let sample_debug_info = create_sample_debug_info();

        let input = "10";
        let expected = vec![
            NUMBER(sample_debug_info.clone(), 10),
        ];

        let output = lexer(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_type_names() {
        let sample_debug_info = create_sample_debug_info();

        let input = "number";
        let expected = vec![
            NUMBER_TYPE(sample_debug_info.clone()),
        ];

        let output = lexer(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_ident() {
        let sample_debug_info = create_sample_debug_info();

        let input = "identifier";
        let expected = vec![
            IDENT(sample_debug_info, "identifier".to_string())
        ];

        let output = lexer(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_keyword() {
        let sample_debug_info = create_sample_debug_info();

        let input = "const func";
        let expected = vec![
            CONST(sample_debug_info.clone()),
            FUNC(sample_debug_info.clone()),
        ];

        let output = lexer(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_add_func() {
        let sample_debug_info = create_sample_debug_info();

        let input = "
            fn add(a: number, b: number) {
                return a + b;
            }

            const total = add(1,2);
        ";

        let expected = vec![
            // fn add(a: number, b: number) {
            FUNC(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "add".to_string()),
            LPAREN(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "a".to_string()),
            COLON(sample_debug_info.clone()),
            NUMBER_TYPE(sample_debug_info.clone()),
            COMMA(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "b".to_string()),
            COLON(sample_debug_info.clone()),
            NUMBER_TYPE(sample_debug_info.clone()),
            RPAREN(sample_debug_info.clone()),
            LBRACE(sample_debug_info.clone()),
            // return a + b;
            RETURN(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "a".to_string()),
            PLUS(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "b".to_string()),
            SEMICOLON(sample_debug_info.clone()),
            // }
            RBRACE(sample_debug_info.clone()),
            // const total = add(1,2);
            CONST(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "total".to_string()),
            ASSIGN(sample_debug_info.clone()),
            IDENT(sample_debug_info.clone(), "add".to_string()),
            LPAREN(sample_debug_info.clone()),
            NUMBER(sample_debug_info.clone(), 1),
            COMMA(sample_debug_info.clone()),
            NUMBER(sample_debug_info.clone(), 2),
            RPAREN(sample_debug_info.clone()),
            COLON(sample_debug_info.clone()),
        ];

        let output = lexer(input);
        assert_eq!(output, expected);


    }

}