use std::hash::Hash;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct TypedIdent {
    name: String,
}

impl TypedIdent {
    pub fn new(ident_name: String) -> TypedIdent {
        TypedIdent {
            name: ident_name
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeFlag {
    NumberType, // x: number
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypedAstType {
    Number,
    Func(Vec<TypedAstType>, Option<Box<TypedAstType>>), // Vec<TypedAstType> -> func args, second TypedAstType -> return type,
    Void,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypedNumber {
    num: i32
}

impl TypedNumber {
    pub fn new(num: i32) -> TypedNumber {
        TypedNumber {
            num
        }
    }

    pub fn get_num(&self) -> i32 {
        self.num.clone()
    }

}

#[derive(Debug, Eq, PartialEq, Clone)]
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

    pub fn get_func_name(&self) -> TypedIdent {
        self.func_name.clone()
    }

    pub fn get_args(&self) -> Vec<TypedExpr> {
        self.args.to_vec()
    }

}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypedExpr {
    CallExpr(TypedAstType, TypedCallExpr), // add(1, 1 + 1)
    NumExpr(TypedAstType, TypedNumber), // 1
    NumIdentExpr(TypedAstType, TypedIdent), // x
    NumAddExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 1 + 2
}

impl TypedExpr {
    pub fn get_typed_ast_type(&self) -> TypedAstType {
        match self {
            TypedExpr::CallExpr(typed_ast_type,_) => {
                typed_ast_type.clone()
            },
            TypedExpr::NumExpr(typed_ast_type,_) => {
                typed_ast_type.clone()
            },
            TypedExpr::NumIdentExpr(typed_ast_type, _) => {
                typed_ast_type.clone()
            },
            TypedExpr::NumAddExpr(typed_ast_type, _, _) => {
                typed_ast_type.clone()
            }
        }
    }
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

    pub fn get_name(&self) -> TypedIdent {
        self.name.clone()
    }

    pub fn get_type_name(&self) -> Option<TypeFlag> {
        self.type_name.clone()
    }

    pub fn get_value(&self) -> Option<TypedExpr> {
        self.value.clone()
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

    pub fn get_name(&self) -> TypedIdent {
        self.name.clone()
    }

    pub fn get_arg_type(&self) -> TypeFlag {
        self.arg_type.clone()
    }

}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypedReturnStmt {
    expr: TypedExpr,
}

impl TypedReturnStmt {
    pub fn new(expr: TypedExpr) -> TypedReturnStmt {
        TypedReturnStmt {
            expr
        }
    }

    pub fn get_expr(&self) -> TypedExpr {
        self.expr.clone()
    }

}

#[derive(Debug, Eq, PartialEq)]
pub struct TypedFunc {
    name: TypedIdent,
    args: Vec<TypedFuncArg>,
    stmts: Vec<TypedStmt>,
    return_stmt: Option<TypedReturnStmt>,
}

impl TypedFunc {
    pub fn new(name: TypedIdent, args: Vec<TypedFuncArg>, stmts: Vec<TypedStmt>, return_stmt: Option<TypedReturnStmt>) -> TypedFunc {
        TypedFunc {
            name,
            args,
            stmts,
            return_stmt
        }
    }

    pub fn get_name(&self) -> TypedIdent {
        self.name.clone()
    }

    pub fn get_args(&self) -> Vec<TypedFuncArg> {
        self.args.to_vec()
    }

    pub fn get_stmts(&self) -> Vec<TypedStmt> {
        self.stmts.to_vec()
    }

    pub fn get_return_stmt(&self) -> Option<TypedReturnStmt> {
        self.return_stmt.clone()
    }


}

#[derive(Debug, Eq, PartialEq)]
pub enum TypedStmt {
    VariableDeclaration(TypedVariableDeclaration),
    ExprStmt(TypedExpr),
    Func(TypedFunc)
}
