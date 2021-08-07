#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Ident,
    pub typeName: Option<Types>,
    pub value: Option<Expr>
}

#[derive(Debug)]
pub enum Types {
    NumberType,
}

#[derive(Debug)]
pub struct FuncArg {
    pub name: Ident,
    pub argType: Types,
}

#[derive(Debug)]
pub struct Func {
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub stmts: Vec<Stmt>,
    pub returnStmt: Option<ReturnStmt>,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Op(Box<Expr>, Opcode, Box<Expr>),
    Ident(Ident),
}

#[derive(Debug)]
pub enum Opcode {
    Add,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
}
