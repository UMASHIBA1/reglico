use super::ast;
use crate::parser::ast::AstNode;
use std::fmt::Error;
use once_cell::sync::Lazy;
use lazy_static::lazy_static;

use pest::Parser;
use pest::iterators::{Pairs, Pair};
use pest::prec_climber::*;

#[derive(Parser)]
#[grammar = "parser/reglico.pest"]
struct ReglicoParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
    use Rule::*;
    use Assoc::*;
    PrecClimber::new(vec![
            Operator::new(Rule::),
        Operator::new(Rule::verb, Left)
    ])
};
}

pub fn eval(program: Pairs<Rule>) -> () {
    PREC_CLIMBER.climb(program,
                       |pair: Pair<Rule>| match pair.as_rule() {
                           _ => pair
                       },
                       |lhs: f64, op: Pair<Rule>, rhs: f64| match op.as_rule() {
                           _ => unreachable!(),
                       },
    );
}

pub fn parse(source: &str) -> () {

    let parse_result = ReglicoParser::parse(Rule::program, source);
    match parse_result {
        Ok(program) => eval(program),
        Err(_) => println!("Syntax Error!")
    }

}


#[cfg(test)]
mod tests {
    use crate::parser::parser::parse;
    use pest::iterators::Pairs;

    #[test]
    fn const_assign_test() {

        assert_eq!(parse("
    fn add(a: number, b: number) {
        return
        a + b;
    }

    const total = add(1,2);
    "), parse("
        fn mul(a: number, b: number) {
        return
        a + b;
    }

    const total = add(3,2);
    "))

    }
}
