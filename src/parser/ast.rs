pub enum DyadicVerb {
    Equal,
    Plus,
}

pub enum AstNode {
    Ident(String),
    DyadicOp {
        verb: DyadicVerb,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Number(i32)
}