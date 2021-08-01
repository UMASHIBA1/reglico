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
pub enum Expr {
    Number(i32),
}