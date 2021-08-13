#[derive(Debug, Eq, PartialEq)]
pub struct Ident {
    name: String,
}

impl Ident {
    pub fn new(ident_name: String) -> Ident {
        Ident {
            name: ident_name
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct VariableDeclaration {
    name: Ident,
    typeName: Option<Types>,
    value: Option<Expr>
}

impl VariableDeclaration {
    pub fn new(name: Ident, typeName: Option<Types>, value: Option<Expr>) -> VariableDeclaration {
        VariableDeclaration {
            name,
            typeName,
            value,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Types {
    NumberType,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FuncArg {
    name: Ident,
    argType: Types,
}

impl FuncArg {
    pub fn new(name: Ident, argType: Types) -> FuncArg {
        FuncArg {
            name,
            argType,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStmt {
    expr: Expr,
}

impl ReturnStmt {
    pub fn new(expr: Expr) -> ReturnStmt {
        ReturnStmt {
            expr
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Func {
    name: Ident,
    args: Vec<FuncArg>,
    stmts: Vec<Stmt>,
    returnStmt: Option<ReturnStmt>,
}

impl Func {
    pub fn new(name: Ident, args: Vec<FuncArg>, stmts: Vec<Stmt>, returnStmt: Option<ReturnStmt>) -> Func {
        Func {
            name,
            args,
            stmts,
            returnStmt,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallExpr {
    func_name: Ident,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(func_name: Ident, args: Vec<Expr>) -> CallExpr {
        CallExpr {
            func_name,
            args
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Number {
    num: i32
}

impl Number {
    pub fn new(num: i32) -> Number {
        Number {
            num
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
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
            r_expr: Box::new(r_expr)
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
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

#[derive(Debug, Eq, PartialEq)]
pub enum Opcode {
    Add,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprStmt {
    expr: Expr,
}

impl ExprStmt {
    pub fn new(expr: Expr) -> ExprStmt {
        ExprStmt {
            expr
        }
    }
}


#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    VariableDeclaration(VariableDeclaration),
    ExprStmt(ExprStmt),
    Func(Func),
}

impl Stmt {
    // NOTE: {???}_new is shortcut method for creating Stmt.
    //

    pub fn var_new(name: Ident, typeName: Option<Types>, value: Option<Expr>) -> Stmt {
        Stmt::VariableDeclaration(
            VariableDeclaration::new(name, typeName, value)
        )
    }

    pub fn expr_new(expr: Expr) -> Stmt {
        Stmt::ExprStmt(
            ExprStmt::new(expr)
        )
    }

    pub fn func_new(name: Ident, args: Vec<FuncArg>, stmts: Vec<Stmt>, returnStmt: Option<ReturnStmt>) -> Stmt {
        Stmt::Func(Func::new(
            name,
            args,
            stmts,
            returnStmt
        ))
    }

}