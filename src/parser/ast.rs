#[derive(Debug, Eq, PartialEq)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct VariableDeclaration {
    pub name: Ident,
    pub typeName: Option<Types>,
    pub value: Option<Expr>
}

#[derive(Debug, Eq, PartialEq)]
pub enum Types {
    NumberType,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FuncArg {
    pub name: Ident,
    pub argType: Types,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallExpr {
    pub ident: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Func {
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub stmts: Vec<Stmt>,
    pub returnStmt: Option<ReturnStmt>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Number(i32),
    Op(Box<Expr>, Opcode, Box<Expr>),
    Ident(Ident),
    CallExpr(CallExpr),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Opcode {
    Add,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
}


impl Expr {
    pub fn op_new(l_expr: Expr, op: Opcode, r_expr: Expr) -> Expr {
        Expr::Op(
            Box::new(l_expr),
            op,
            Box::new(r_expr)
        )
    }


}