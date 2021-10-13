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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct BlockBox {
    stmts: Vec<Stmt>,
}

impl BlockBox {
    pub fn new(stmts: Vec<Stmt>) -> BlockBox {
        BlockBox {
            stmts
        }
    }

    pub fn get_stmts(&self) -> Vec<Stmt> {
        self.stmts.to_vec()
    }

}

#[derive(Debug, PartialEq, Clone)]
pub enum Types {
    NumberType,
    BoolType,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct Number {
    num: i64,
    raw_num_string: String,
}

impl Number {
    pub fn new(num: i64, raw_num_str: &str) -> Number {
        let raw_num_string = raw_num_str.to_string();
        Number { num,  raw_num_string}
    }

    pub fn get_num(&self) -> i64 {
        self.num
    }

    pub fn get_raw_num_string(&self) -> String {
        self.raw_num_string.clone()
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    bool: bool,
}

impl Boolean {
    pub fn new(bool: bool) -> Boolean {Boolean {bool}}

    pub fn get_bool(&self) -> bool { self.bool }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpr {
    stmts: Vec<Stmt>,
}

impl BlockExpr {
    pub fn new(stmts: Vec<Stmt>) -> BlockExpr {
        BlockExpr {
            stmts,
        }
    }

    pub fn get_stmts(&self) -> Vec<Stmt> {
        self.stmts.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Bool(Boolean),
    Num(Number),
    Op(Operation),
    Ident(Ident),
    Call(CallExpr),
    Block(BlockExpr),
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

    pub fn num_new(num: i64, raw_num_str: &str) -> Expr {
        Expr::Num(Number::new(num, raw_num_str))
    }

    pub fn call_new(func_name: Ident, args: Vec<Expr>) -> Expr {
        Expr::Call(CallExpr::new(func_name, args))
    }

    pub fn block_new(stmts: Vec<Stmt>) -> Expr {
        Expr::Block(BlockExpr::new(stmts))
    }

    pub fn ident_new(ident: Ident) -> Expr {
        Expr::Ident(ident)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Opcode {
    Add, // +
    Sub, // -
    Mul, // *
    LessThanOrEqual, // <=
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum CanElseStmt {
    BlockBox(BlockBox),
    IfStmt(IfStmt),
}

impl CanElseStmt {
    pub fn block_box_new(stmts: Vec<Stmt>) -> CanElseStmt {
        CanElseStmt::BlockBox(BlockBox::new(stmts))
    }

    pub fn if_stmt_new(condition_expr: Expr, then_stmt: BlockBox, else_stmt: Option<CanElseStmt>) -> CanElseStmt {
        CanElseStmt::IfStmt(IfStmt::new(condition_expr, then_stmt, else_stmt))
    }

    pub fn new_from_if_stmt(if_stmt: IfStmt) -> CanElseStmt {
        CanElseStmt::IfStmt(if_stmt)
    }

    pub fn new_from_block_box(block_box: BlockBox) -> CanElseStmt {
        CanElseStmt::BlockBox(block_box)
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    condition_expr: Expr,
    then_stmt: BlockBox,
    else_stmt: Option<Box<CanElseStmt>>,
}

impl IfStmt {
    pub fn new(condition_expr: Expr, then_stmt: BlockBox, else_stmt: Option<CanElseStmt>) -> IfStmt {
        let else_stmt = match else_stmt {
            Some(can_else_stmt) => Some(Box::new(can_else_stmt)),
            None => None,
        };
        IfStmt {
            condition_expr,
            then_stmt,
            else_stmt,
        }
    }

    pub fn get_condition_expr(&self) -> Expr {
        self.condition_expr.clone()
    }

    pub fn get_then_stmt(&self) -> BlockBox {
        self.then_stmt.clone()
    }

    pub fn get_else_stmt(&self) -> Option<Box<CanElseStmt>> {
        self.else_stmt.clone()
    }

}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
    ReturnStmt(ReturnStmt),
    IfStmt(IfStmt)
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

    pub fn if_stmt(condition_expr: Expr, then_stmt: BlockBox, else_stmt: Option<CanElseStmt>) -> Stmt {
        Stmt::IfStmt(IfStmt::new(condition_expr, then_stmt, else_stmt))
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
