#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Ident {
    name: String,
}

impl Ident {
    pub fn new(ident_name: String) -> Ident {
        Ident { name: ident_name }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct VariableDeclaration {
    name: Ident,
    type_name: Option<Types>,
    value: Option<Expr>,
}

impl VariableDeclaration {
    pub fn new(name: Ident, type_name: Option<Types>, value: Option<Expr>) -> VariableDeclaration {
        VariableDeclaration {
            name,
            type_name,
            value,
        }
    }

    pub fn get_var_name(&self) -> Ident {
        self.name.clone()
    }

    pub fn get_type_name(&self) -> Option<Types> {
        self.type_name.clone()
    }

    pub fn get_value(&self) -> Option<Expr> {
        self.value.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Types {
    NumberType,
    BoolType,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FuncArg {
    name: Ident,
    arg_type: Types,
}

impl FuncArg {
    pub fn new(name: Ident, arg_type: Types) -> FuncArg {
        FuncArg { name, arg_type }
    }

    pub fn get_name(&self) -> Ident {
        self.name.clone()
    }

    pub fn get_arg_type(&self) -> Types {
        self.arg_type.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnStmt {
    expr: Expr,
}

impl ReturnStmt {
    pub fn new(expr: Expr) -> ReturnStmt {
        ReturnStmt { expr }
    }

    pub fn get_expr(&self) -> Expr {
        self.expr.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Func {
    name: Ident,
    args: Vec<FuncArg>,
    stmts: Vec<Stmt>,
}

impl Func {
    pub fn new(name: Ident, args: Vec<FuncArg>, stmts: Vec<Stmt>) -> Func {
        Func { name, args, stmts }
    }

    pub fn get_name(&self) -> Ident {
        self.name.clone()
    }

    pub fn get_func_args(&self) -> Vec<FuncArg> {
        self.args.to_vec()
    }

    pub fn get_stmts(&self) -> Vec<Stmt> {
        self.stmts.to_vec()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallExpr {
    func_name: Ident,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(func_name: Ident, args: Vec<Expr>) -> CallExpr {
        CallExpr { func_name, args }
    }

    pub fn get_func_name(&self) -> Ident {
        self.func_name.clone()
    }

    pub fn get_args(&self) -> Vec<Expr> {
        self.args.to_vec()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Number {
    num: i32,
}

impl Number {
    pub fn new(num: i32) -> Number {
        Number { num }
    }

    pub fn get_num(&self) -> i32 {
        self.num
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Boolean {
    bool: bool,
}

impl Boolean {
    pub fn new(bool: bool) -> Boolean {Boolean {bool}}

    pub fn get_bool(&self) -> bool { self.bool }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Operation {
    opcode: Opcode,
    l_expr: Box<Expr>,
    r_expr: Box<Expr>,
}

impl Operation {
    pub fn new(l_expr: Expr, opcode: Opcode, r_expr: Expr) -> Operation {
        Operation {
            l_expr: Box::new(l_expr),
            opcode,
            r_expr: Box::new(r_expr),
        }
    }

    pub fn get_operation(&self) -> (Expr, Opcode, Expr) {
        (
            *self.l_expr.clone(),
            self.opcode.clone(),
            *self.r_expr.clone(),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Bool(Boolean),
    Num(Number),
    Op(Operation),
    Ident(Ident),
    Call(CallExpr),
}

impl Expr {
    // NOTE: {???}_new is shortcut method for creating Expr.
    // Expr::Op(Operation::new(Expr::Num(Number::new(10)), Opcode::Add, Expr::Num(Number::new(20))))
    // ↓
    // Expr::op_new(Expr::num_new(10), Opcode::Add, Expr::num_new(20))

    pub fn op_new(l_expr: Expr, op: Opcode, r_expr: Expr) -> Expr {
        Expr::Op(Operation::new(l_expr, op, r_expr))
    }

    pub fn bool_new(bool: bool) -> Expr { Expr::Bool(Boolean::new(bool)) }

    pub fn num_new(num: i32) -> Expr {
        Expr::Num(Number::new(num))
    }

    pub fn call_new(func_name: Ident, args: Vec<Expr>) -> Expr {
        Expr::Call(CallExpr::new(func_name, args))
    }

    pub fn ident_new(ident: Ident) -> Expr {
        Expr::Ident(ident)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExprStmt {
    expr: Expr,
}

impl ExprStmt {
    pub fn new(expr: Expr) -> ExprStmt {
        ExprStmt { expr }
    }

    pub fn get_expr(&self) -> Expr {
        self.expr.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
    ReturnStmt(ReturnStmt),
}

impl Stmt {
    pub fn var_new(name: Ident, type_name: Option<Types>, value: Option<Expr>) -> Stmt {
        Stmt::VariableDeclaration(VariableDeclaration::new(name, type_name, value))
    }

    pub fn expr_new(expr: Expr) -> Stmt {
        Stmt::ExprStmt(ExprStmt::new(expr))
    }

    pub fn func_new(name: Ident, args: Vec<FuncArg>, stmts: Vec<Stmt>) -> Stmt {
        Stmt::Func(Func::new(name, args, stmts))
    }

    pub fn return_new(expr: Expr) -> Stmt {
        Stmt::ReturnStmt(ReturnStmt::new(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::Ident;

    #[test]
    fn test_ident_get_name_can_multi_call() {
        let ident = Ident::new("tmp".to_string());
        let ident_name = ident.get_name();
        assert_eq!(ident_name, "tmp".to_string());
        let ident_name2 = ident.get_name();
        assert_eq!(ident_name2, "tmp".to_string());
    }
}
