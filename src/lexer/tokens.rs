use std::fmt::Debug;

#[derive(Debug, PartialEq, Clone)]
pub struct DebugInfo {
    filename: String,
    row_num: i32,
}

impl DebugInfo {
    pub fn new(filename: &str, row_num: i32) -> Self {
        Self {
            filename: String::from(filename),
            row_num
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // Special Token
    ILLEGAL(DebugInfo),
    EOF(DebugInfo),
    // Type Token
    NUMBER(DebugInfo,i32),
    // Type Name Token
    NUMBER_TYPE(DebugInfo),
    // Identifier Token
    IDENT(DebugInfo, String),
    // Operator Token
    PLUS(DebugInfo), // +
    ASSIGN(DebugInfo), // =
    // Delimiter Token
    COMMA(DebugInfo), // ,
    SEMICOLON(DebugInfo), // ;
    COLON(DebugInfo), // :
    LPAREN(DebugInfo), // (
    RPAREN(DebugInfo), // )
    LBRACE(DebugInfo), // {
    RBRACE(DebugInfo), // }
    // Keyword Token
    FUNC(DebugInfo),
    CONST(DebugInfo),
    RETURN(DebugInfo),
}
