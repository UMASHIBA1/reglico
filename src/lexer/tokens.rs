enum SpecialToken {
    ILLEGAL,
    EOF,
}

enum TypeToken {
    NUMBER(i32)
}

pub enum Token {
    // Special Token
    ILLEGAL,
    EOF,
    // Type Token
    NUMBER(i32),
    // Identifier Token
    IDENT,
    // Operator Token
    PLUG,
    ASSIGN,
    // Delimiter Token
    COMMA, // ,
    SEMICOLON, // ;
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    // Keyword Token
    FUNC,
    CONST,
}