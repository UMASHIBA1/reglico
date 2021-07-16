use super::tokens::Token;
use crate::lexer::tokens::{DebugInfo, KEYWORDS};
use crate::lexer::tokens::Token::{ASSIGN, PLUS, COMMA, SEMICOLON, COLON, LPAREN, RPAREN, LBRACE, RBRACE, ILLEGAL, IDENT, NUMBER, EOF};

pub struct Lexer<'a> {
    input: &'a str,
    now_position: usize,
    next_position: usize, // for confirm next char
    ch: char,
    now_line: usize,
    file_name: &'a str,
}

impl Lexer<'_> {
    fn new<'a>(input: &'a str, file_name: &'a str) -> Lexer<'a> {
        let ch = input.chars().nth(0).unwrap_or('\0');
        Lexer {
            input,
            now_position: 0,
            next_position: 1,
            ch,
            now_line: 1,
            file_name,
        }
    }

    fn create_debug_info(&self) -> DebugInfo { DebugInfo::new(Some(self.file_name), Some((self.now_line)))}

    pub fn lexing(code: &str, file_name: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(code, file_name);
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
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.next_position).unwrap_or('\0');
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



    fn consume_whitespace(&mut self) {
        loop {
            if self.ch != ' ' && self.ch != '\t' && self.ch != '\r' && self.ch != '\n' {
                break;
            }

            if self.ch == '\n' {
                self.now_line += 1;
            }

            self.read_char();

        }
    }

    fn is_number(ch: char) -> bool {
        '0' <= ch && ch <= '9'
    }

    fn read_number(&mut self) -> Result<i32, String>  {
        let start_position = self.now_position;
        while Lexer::is_number(self.ch) {
            self.read_char();
        }

        let str_number = &self.input[start_position..self.now_position];
        match str_number.parse::<i32>() {
            Ok(num) => Ok(num),
            Err(_) => Err(str_number.to_string())
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();
        let now_debug_info = self.create_debug_info();
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
            '\0' => EOF(now_debug_info),
            _ => {
                if Lexer::is_letter(self.ch) {
                    let identifier = self.read_identifier();
                    return Lexer::lookup_keyword(identifier, now_debug_info);
                }else if Lexer::is_number(self.ch) {
                    return match self.read_number() {
                        Ok(num) => NUMBER(now_debug_info, num),
                        Err(str_num) => ILLEGAL(now_debug_info, str_num)
                    };
                }
                else {
                    ILLEGAL(now_debug_info, self.ch.to_string())
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
    use crate::lexer::tokens::Token::EOF;

    const FILE_NAME: &str = "tmp_file";

    fn create_debug_info(file_name: &str, now_line: usize) -> DebugInfo { DebugInfo::new(Some(file_name), Some((now_line)))}

    #[test]
    fn tokenize_delimiter() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = ",;(){}";
        let expected = vec![
            COMMA(debug_info.clone()),
            SEMICOLON(debug_info.clone()),
            LPAREN(debug_info.clone()),
            RPAREN(debug_info.clone()),
            LBRACE(debug_info.clone()),
            RBRACE(debug_info),
        ];

        let output = Lexer::lexing(input, FILE_NAME);
        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_operator() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = "+=";
        let expected = vec![
            PLUS(debug_info.clone()),
            ASSIGN(debug_info)
        ];

        let output = Lexer::lexing(input, FILE_NAME);
        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_number() {
        let debug_info = create_debug_info(FILE_NAME, 1);
        let input = "10";
        let expected = vec![
            NUMBER(debug_info.clone(), 10),
        ];

        let output = Lexer::lexing(input, FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_type_names() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = "number";
        let expected = vec![
            NUMBER_TYPE(debug_info.clone()),
        ];

        let output = Lexer::lexing(input,  FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_ident() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = "identifier";
        let expected = vec![
            IDENT(debug_info, "identifier".to_string())
        ];

        let output = Lexer::lexing(input,  FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_keyword() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = "const fn";
        let expected = vec![
            CONST(debug_info.clone()),
            FUNC(debug_info.clone()),
        ];

        let output = Lexer::lexing(input, FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn test_debug_info() {
        let debug_info1 = create_debug_info(FILE_NAME, 1);
        let debug_info2 = create_debug_info(FILE_NAME, 2);
        let debug_info3 = create_debug_info(FILE_NAME, 3);

        let input = "const fn
        fn
        }";
        let expected = vec![
            CONST(debug_info1.clone()),
            FUNC(debug_info1.clone()),
            FUNC(debug_info2.clone()),
            RBRACE(debug_info3.clone())
        ];

        let output = Lexer::lexing(input, FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn test_eof() {
        let debug_info = create_debug_info(FILE_NAME, 1);

        let input = "const fn } \0";
        let expected = vec![
            CONST(debug_info.clone()),
            FUNC(debug_info.clone()),
            RBRACE(debug_info.clone()),
            EOF(debug_info.clone())
        ];

        let output = Lexer::lexing(input, FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn test_eof_with_no_sign() {
        let debug_info1 = create_debug_info(FILE_NAME, 1);
        let debug_info2 = create_debug_info(FILE_NAME, 2);

        let input = "const fn }
        ";
        let expected = vec![
            CONST(debug_info1.clone()),
            FUNC(debug_info1.clone()),
            RBRACE(debug_info1.clone()),
            EOF(debug_info2.clone())
        ];

        let output = Lexer::lexing(input, FILE_NAME);

        assert_eq!(output, expected);
    }

    #[test]
    fn tokenize_add_func() {
        let debug_info1 = create_debug_info(FILE_NAME, 1);
        let debug_info2 = create_debug_info(FILE_NAME, 2);
        let debug_info3 = create_debug_info(FILE_NAME, 3);
        let debug_info5 = create_debug_info(FILE_NAME, 5);
        let debug_info6 = create_debug_info(FILE_NAME, 6);

        let input = "fn add(a: number, b: number) {
                return a + b;
            }

            const total = add(1,2);
            ";

        let expected = vec![
            // fn add(a: number, b: number) {
            FUNC(debug_info1.clone()),
            IDENT(debug_info1.clone(), "add".to_string()),
            LPAREN(debug_info1.clone()),
            IDENT(debug_info1.clone(), "a".to_string()),
            COLON(debug_info1.clone()),
            NUMBER_TYPE(debug_info1.clone()),
            COMMA(debug_info1.clone()),
            IDENT(debug_info1.clone(), "b".to_string()),
            COLON(debug_info1.clone()),
            NUMBER_TYPE(debug_info1.clone()),
            RPAREN(debug_info1.clone()),
            LBRACE(debug_info1.clone()),
            // return a + b;
            RETURN(debug_info2.clone()),
            IDENT(debug_info2.clone(), "a".to_string()),
            PLUS(debug_info2.clone()),
            IDENT(debug_info2.clone(), "b".to_string()),
            SEMICOLON(debug_info2.clone()),
            // }
            RBRACE(debug_info3.clone()),
            // const total = add(1,2);
            CONST(debug_info5.clone()),
            IDENT(debug_info5.clone(), "total".to_string()),
            ASSIGN(debug_info5.clone()),
            IDENT(debug_info5.clone(), "add".to_string()),
            LPAREN(debug_info5.clone()),
            NUMBER(debug_info5.clone(), 1),
            COMMA(debug_info5.clone()),
            NUMBER(debug_info5.clone(), 2),
            RPAREN(debug_info5.clone()),
            SEMICOLON(debug_info5.clone()),
            EOF(debug_info6.clone()),
        ];

        let output = Lexer::lexing(input, FILE_NAME);
        assert_eq!(output, expected);
    }




}