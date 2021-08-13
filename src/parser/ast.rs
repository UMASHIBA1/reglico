#[derive(Debug, Eq, PartialEq)]
pub struct Ident {
    name: String,
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
pub struct Func {
    pub name: Ident,
    pub args: Vec<FuncArg>,
    pub stmts: Vec<Stmt>,
    pub returnStmt: Option<ReturnStmt>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallExpr {
    func_name: Ident,
    args: Vec<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Number {
    num: i32
}

#[derive(Debug, Eq, PartialEq)]
pub struct Operation {
    opcode: Opcode,
    l_expr: Box<Expr>,
    r_expr: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Num(Number),
    Op(Operation),
    Ident(Ident),
    Call(CallExpr),
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

impl Number {
    pub fn new(num: i32) -> Number {
        Number {
            num
        }
    }
}

impl Operation {
    pub fn new(l_expr: Expr, opcode: Opcode, r_expr: Expr) -> Operation {
        Operation {
            l_expr: Box::new(l_expr),
            opcode,
            r_expr: Box::new(r_expr)
        }
    }
}

impl Ident {
    pub fn new(ident_name: String) -> Ident {
        Ident {
            name: ident_name
        }
    }
}

impl CallExpr {
    pub fn new(func_name: Ident, args: Vec<Expr>) -> CallExpr {
        CallExpr {
            func_name,
            args
        }
    }
}

impl Expr {
    // NOTE: {???}_new is shortcut method for creating Expr.
    // Expr::Op(Operation::new(Expr::Num(Number::new(10)), Opcode::Add, Expr::Num(Number::new(20))))
    // ↓
    // Expr::op_new(Expr::num_new(10), Opcode::Add, Expr::num_new(20))
    pub fn op_new(l_expr: Expr, op: Opcode, r_expr: Expr) -> Expr {
        Expr::Op(
            Operation::new(l_expr, op, r_expr)
        )
    }

    pub fn num_new(num: i32) -> Expr {
        Expr::Num(
            Number::new(num)
        )
    }

    pub fn call_new(func_name: Ident, args: Vec<Expr>) -> Expr {
        Expr::Call(
            CallExpr::new(
                func_name,
                args
            )
        )
    }

    pub fn ident_new(ident: Ident) -> Expr {
        Expr::Ident(
            ident
        )
    }


}