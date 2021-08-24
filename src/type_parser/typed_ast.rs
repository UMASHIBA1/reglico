#[derive(Debug, Eq, PartialEq)]
pub struct TypedIdent {
    name: String,
}

impl TypedIdent {
    pub fn new(ident_name: String) -> TypedIdent {
        TypedIdent {
            name: ident_name
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeFlag {
    NumberType, // x: number
}


#[derive(Debug, Eq, PartialEq)]
pub enum TypedAstType {
    Number,
    Func(Vec<TypedAstType>, Box<TypedAstType>), // Vec<TypedAstType> -> func args, second TypedAstType -> return type
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedNumber {
    num: i32
}

impl TypedNumber {
    pub fn new(num: i32) -> TypedNumber {
        TypedNumber {
            num
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedCallExpr {
    func_name: TypedIdent,
    args: Vec<TypedExpr>
}

impl TypedCallExpr {
    pub fn new(func_name: TypedIdent, args: Vec<TypedExpr>) -> TypedCallExpr {
        TypedCallExpr {
            func_name,
            args
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypedExpr {
    CallExpr(TypedAstType, TypedCallExpr), // add(1, 1 + 1)
    NumExpr(TypedAstType, TypedNumber), // 1
    NumIdentExpr(TypedAstType, TypedIdent), // x
    NumAddExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 1 + 2
}


#[derive(Debug, Eq, PartialEq)]
pub struct TypedVariableDeclaration {
    name: TypedIdent,
    type_name: Option<TypeFlag>,
    value: Option<TypedExpr>
}

impl TypedVariableDeclaration {
    pub fn new(name: TypedIdent, type_name: Option<TypeFlag>, value: Option<TypedExpr>) -> TypedVariableDeclaration {
        TypedVariableDeclaration {
            name,
            type_name,
            value,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedFuncArg {
    name: TypedIdent,
    arg_type: TypeFlag,
}

impl TypedFuncArg {
    pub fn new(name: TypedIdent, arg_type: TypeFlag) -> TypedFuncArg {
        TypedFuncArg {
            name,
            arg_type,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedReturnStmt {
    expr: TypedExpr,
}

impl TypedReturnStmt {
    pub fn new(expr: TypedExpr) -> TypedReturnStmt {
        TypedReturnStmt {
            expr
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedFunc {
    name: TypedIdent,
    args: Vec<TypedFuncArg>,
    stmts: Vec<TypedStmt>,
    return_stmt: Option<TypedReturnStmt>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypedStmt {
    VariableDeclaration(TypedVariableDeclaration),
    ExprStmt(TypedExpr),
    Func(TypedFunc)
}
