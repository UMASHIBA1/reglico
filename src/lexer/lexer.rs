use super::tokens::Token;
use crate::lexer::tokens::{DebugInfo, KEYWORDS};
use crate::lexer::tokens::Token::{ASSIGN, PLUS, COMMA, SEMICOLON, COLON, LPAREN, RPAREN, LBRACE, RBRACE, ILLEGAL, IDENT};

pub struct Lexer<'a> {
    input: &'a str,
    now_position: usize,
    next_position: usize, // for confirm next char
    ch: char
}

impl Lexer<'_> {
    fn new(input: &str) -> Lexer {
        let ch = input.chars().nth(0).unwrap_or('0');
        Lexer {
            input,
            now_position: 0,
            next_position: 1,
            ch,
        }
    }

    fn create_sample_debug_info() -> DebugInfo {
        DebugInfo::new(Some("tmp"), Some(1))
    }

    pub fn lexing(code: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(code);
        let len = lexer.input.len();
        let mut tokens = Vec::new();
        loop {
            if lexer.next_position > lexer.input.len() {
                break;
            }
            let token = lexer.next_token();
            tokens.push(token);
        }
        tokens
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.ch = '0';
        } else {
            self.ch = self.input.chars().nth(self.next_position).unwrap_or('0');
        }
        self.now_position = self.next_position;
        self.next_position += 1;
    }

    fn is_letter(ch: char) -> bool {
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }

    fn read_identifier(&mut self) -> &str {
        let start = self.now_position;
        loop {
            if !Lexer::is_letter(self.ch) {
                break;
            }
            self.read_char();
        }
        &self.input[start..self.now_position]
    }

    fn lookup_keyword(ident: &str, debug_info: DebugInfo) -> Token {
        let keyword_factory = KEYWORDS.get(ident).cloned();

        match keyword_factory {
            Some(keyword_factory) => {
                let keyword = keyword_factory(debug_info);
                keyword
            },
            None => IDENT(debug_info, String::from(ident)),
        }
    }

    pub fn next_token(&mut self) -> Token {
        // TODO: filenameとrow_numをカウントしてDebugInfoを作成する
        let now_debug_info = Lexer::create_sample_debug_info();
        let token = match self.ch {
            '+' => PLUS(now_debug_info),
            '=' => ASSIGN(now_debug_info),
            ',' => COMMA(now_debug_info),
            ';' => SEMICOLON(now_debug_info),
            ':' => COLON(now_debug_info),
            '(' => LPAREN(now_debug_info),
            ')' => RPAREN(now_debug_info),
            '{' => LBRACE(now_debug_info),
            '}' => RBRACE(now_debug_info),
            _ => {
                if Lexer::is_letter(self.ch) {
                    let identifier = self.read_identifier();
                    Lexer::lookup_keyword(identifier, now_debug_info)
                } else {
                    ILLEGAL(now_debug_info)
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::super::tokens::DebugInfo;
    use super::super::tokens::Token::{COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, PLUS, ASSIGN, NUMBER, CONST, FUNC, IDENT, NUMBER_TYPE, COLON, RETURN};

    fn create_sample_debug_info() -> DebugInfo {
        DebugInfo::new(Some("tmp"), Some(1))
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

        let output = Lexer::lexing(input);
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

        let output = Lexer::lexing(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_types() {
        let sample_debug_info = create_sample_debug_info();

        let input = "10";
        let expected = vec![
            NUMBER(sample_debug_info.clone(), 10),
        ];

        let output = Lexer::lexing(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_type_names() {
        let sample_debug_info = create_sample_debug_info();

        let input = "number";
        let expected = vec![
            NUMBER_TYPE(sample_debug_info.clone()),
        ];

        let output = Lexer::lexing(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_ident() {
        let sample_debug_info = create_sample_debug_info();

        let input = "identifier";
        let expected = vec![
            IDENT(sample_debug_info, "identifier".to_string())
        ];

        let output = Lexer::lexing(input);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_keyword() {
        let sample_debug_info = create_sample_debug_info();

        let input = "const fn";
        let expected = vec![
            CONST(sample_debug_info.clone()),
            FUNC(sample_debug_info.clone()),
        ];

        let output = Lexer::lexing(input);

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

        let output = Lexer::lexing(input);
        assert_eq!(output, expected);


    }

}