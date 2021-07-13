use std::fmt::Debug;
use std::collections::HashMap;
use ::phf::{phf_map, Map};


#[derive(Debug, PartialEq, Clone)]
pub struct DebugInfo {
    filename: Option<String>,
    line_num: Option<i32>,
}

impl DebugInfo {
    pub fn new(filename: Option<&str>, line_num: Option<i32>) -> Self {
        let filename = match filename {
            Some(name) => Some(String::from(name)),
            _ => None
        };
        Self {
            filename,
            line_num
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    // Special Token
    ILLEGAL(DebugInfo, String),
    EOF(DebugInfo),
    // Type Token
    NUMBER(DebugInfo,i32),
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
    FUNC(DebugInfo), // fn
    CONST(DebugInfo), // const
    RETURN(DebugInfo), // return
    // Type Name Token
    NUMBER_TYPE(DebugInfo), // number
}

pub static KEYWORDS: Map<&'static str, fn(DebugInfo) -> Token> = phf_map! {
    "fn" => |debug_info: DebugInfo| Token::FUNC(debug_info),
    "const" => |debug_info: DebugInfo| Token::CONST(debug_info),
    "return" => |debug_info: DebugInfo| Token::RETURN(debug_info),
    "number" => |debug_info: DebugInfo| Token::NUMBER_TYPE(debug_info),
};
