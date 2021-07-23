use super::ast;
use crate::parser::ast::AstNode;
use std::fmt::Error;

use pest::Parser;
use pest::iterators::Pairs;

#[derive(Parser)]
#[grammar = "parser/reglico.pest"]
struct ReglicoParser;

pub fn parse(source: &str) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    // let mut ast = vec![];

    let pairs = ReglicoParser::parse(Rule::const_assignment_expr, source);
    pairs
}


#[cfg(test)]
mod tests {
    use crate::parser::parser::parse;
    use pest::iterators::Pairs;

    #[test]
    fn const_assign_test() {

        assert_eq!(parse("const bbb = 90"), parse("const aaa = 10"))
    }
}