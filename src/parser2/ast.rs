#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub typeName: Option<Types>,
    pub value: Option<Expr>
}

#[derive(Debug)]
pub enum Types {
    NumberType,
}

#[derive(Debug)]
pub struct FuncArg {
    pub name: String,
    pub argType: Types,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<FuncArg>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
}
