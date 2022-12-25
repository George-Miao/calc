use from_pest::FromPest;

use crate::ast::Expr;

mod ast;

#[derive(pest_derive::Parser)]
#[grammar = "../pest/calc.pest"]
pub struct CalcParser;

#[test]
fn test_lexer() {
    use pest::Parser;

    const SRC: &str = "(1 + (10.1001) + (@1))";
    let mut res = CalcParser::parse(Rule::expr, SRC).unwrap();
    let sen = Expr::from_pest(&mut res);
    println!("{sen:#?}");
}
