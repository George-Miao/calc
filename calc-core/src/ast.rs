#![allow(clippy::clone_on_copy)]

use pest::Span;
use pest_ast::FromPest;
use rust_decimal::Decimal;

use crate::Rule;

macro_rules! unit_types {
    ($( $rule:ident: $i:ident ),*) => {
        $(
            #[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[pest_ast(rule(Rule::$rule))]
            pub struct $i;
        )*
    };
}

unit_types! {
    // Unary Op
    neg: Neg,

    // Binary Op
    add: Add,
    sub: Sub,
    mul: Mul,
    div: Div,
    rem: Rem,
    eq: Eq,
    neq: Neq,
    lt: Lt,
    gt: Gt,
    ge: Ge,
    le: Le
}

fn span_into_str(span: Span) -> &str {
    span.as_str()
}

fn parse_reference(str: &str) -> usize {
    str[1..]
        .parse()
        .expect("Input for reference should be `@ ~ ASCII_DIGIT +`")
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::number))]
pub struct Number {
    #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
    pub value: Decimal,
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::literal))]
pub enum Literal {
    #[pest_ast(inner(rule(number)))]
    Number(Number),
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::reference))]
pub struct Reference {
    #[pest_ast(outer(with(span_into_str), with(parse_reference)))]
    pub index: usize,
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::unary_op))]
pub enum UnaryOp {
    Neg(Neg),
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::unary_op_expr))]
pub struct UnaryOpExpr {
    op: UnaryOp,
    expr: Box<Expr>,
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::binary_op))]
pub enum BinaryOps {
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    Rem(Rem),
    Eq(Eq),
    Neq(Neq),
    Lt(Lt),
    Gt(Gt),
    Ge(Ge),
    Le(Le),
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::binary_op_expr))]
pub struct BinaryOpExpr {
    lhs: NominalExpr,
    op: BinaryOps,
    rhs: Box<Expr>,
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::nominal_expr))]
pub enum NominalExpr {
    UnaryOp(UnaryOpExpr),
    Paren(Box<ParenExpr>),
    Reference(Reference),
    Literal(Literal),
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::paren_expr))]
pub struct ParenExpr {
    expr: Expr,
}

#[derive(Debug, FromPest, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pest_ast(rule(Rule::expr))]
pub enum Expr {
    BinaryOp(BinaryOpExpr),
    Nominal(NominalExpr),
}

#[cfg(test)]
mod test {
    use from_pest::FromPest;
    use pest::Parser;
    use rust_decimal::prelude::FromPrimitive;

    const ONE: NominalExpr = NominalExpr::Literal(Literal::Number(Number {
        value: Decimal::ONE,
    }));

    use crate::{ast::*, CalcParser, Rule};
    macro_rules! test_ast {
        ($test_name:ident, $src:literal, $rule:ident, $should_eq:expr) => {
            paste::paste! {
                #[test]
                fn [< test_ $test_name >]() {
                    let mut res = CalcParser::parse(Rule::$rule, $src).unwrap();
                    println!("[{}] Parsed: {:#?}", stringify!($rule), res);
                    let res = FromPest::from_pest(&mut res).unwrap();
                    println!("[{}] Result: {res:#?}", stringify!($rule));
                    assert_eq!($should_eq, res);
                }
            }
        };
    }
    test_ast!(one_plus_one_paren, "(1+1)", paren_expr, {
        ParenExpr {
            expr: Expr::BinaryOp(BinaryOpExpr {
                lhs: ONE,
                op: BinaryOps::Add(Add),
                rhs: Box::new(Expr::Nominal(ONE)),
            }),
        }
    });
    test_ast!(one_plus_one_paren_plus_one, "(1+1)+1", expr, {
        Expr::BinaryOp(BinaryOpExpr {
            lhs: NominalExpr::Paren(Box::new(ParenExpr {
                expr: Expr::BinaryOp(BinaryOpExpr {
                    lhs: ONE,
                    op: BinaryOps::Add(Add),
                    rhs: Box::new(Expr::Nominal(ONE)),
                }),
            })),
            op: BinaryOps::Add(Add),
            rhs: Box::new(Expr::Nominal(ONE)),
        })
    });
    test_ast!(
        floats,
        "10.1001",
        literal,
        Literal::Number(Number {
            value: Decimal::from_f32(10.1001).unwrap()
        })
    );
    test_ast!(one, "1", nominal_expr, { ONE });
    test_ast!(refs, "@10", reference, Reference { index: 10 });
    test_ast!(minus_op, "-", unary_op, UnaryOp::Neg(Neg));
}
