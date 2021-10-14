use std::hash::Hash;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct TypedIdent {
    name: String,
}

impl TypedIdent {
    pub fn new(ident_name: String) -> TypedIdent {
        TypedIdent { name: ident_name }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeFlag {
    NumberType, // x: number
    BoolType, // x: bool
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedAstType {
    Number,
    Bool,
    Func(Vec<TypedAstType>, Box<TypedAstType>), // Vec<TypedAstType> -> func args, second TypedAstType -> return type,
    Void,
    LazyEval,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedNumber {
    num: i64,
    raw_num_string: String,
}

impl TypedNumber {
    pub fn new(num: i64, raw_num_string: String) -> TypedNumber {
        TypedNumber { num, raw_num_string }
    }

    pub fn get_num(&self) -> i64 {
        self.num.clone()
    }

    pub fn get_raw_num_string(&self) -> String {
        self.raw_num_string.clone()
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedBool {
    bool: bool
}

impl TypedBool {
    pub fn new(bool: bool) -> TypedBool { TypedBool {bool} }

    pub fn get_bool(&self) -> bool { self.bool.clone() }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedBlockBox {
    stmts: Vec<TypedStmt>,
    return_ast_type: TypedAstType,
}

impl TypedBlockBox {
    pub fn new(stmts: Vec<TypedStmt>, return_ast_type: TypedAstType) -> TypedBlockBox {
        TypedBlockBox {
            stmts,
            return_ast_type,
        }
    }

    pub fn get_stmts(&self) -> Vec<TypedStmt> {
        self.stmts.to_vec()
    }

    pub fn get_return_ast_type(&self) -> TypedAstType {
        self.return_ast_type.clone()
    }


}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedCallExpr {
    func_name: TypedIdent,
    args: Vec<TypedExpr>,
}

impl TypedCallExpr {
    pub fn new(func_name: TypedIdent, args: Vec<TypedExpr>) -> TypedCallExpr {
        TypedCallExpr { func_name, args }
    }

    pub fn get_func_name(&self) -> TypedIdent {
        self.func_name.clone()
    }

    pub fn get_args(&self) -> Vec<TypedExpr> {
        self.args.to_vec()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedBlock {
    stmts: Vec<TypedStmt>,
}

impl TypedBlock {
    pub fn new(stmts: Vec<TypedStmt>) -> TypedBlock {
        TypedBlock {stmts}
    }

    pub fn get_stmts(&self) -> Vec<TypedStmt> {
        self.stmts.to_vec()
    }

}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    CallExpr(TypedAstType, TypedCallExpr),  // add(1, 1 + 1)
    NumExpr(TypedAstType, TypedNumber),     // 1
    BoolExpr(TypedAstType, TypedBool),      // true or false
    NumIdentExpr(TypedAstType, TypedIdent), // x
    BoolIdentExpr(TypedAstType, TypedIdent), // x
    NumBlockExpr(TypedAstType, TypedBlock), // {return 1;}
    BoolBlockExpr(TypedAstType, TypedBlock), // {return true;}
    VoidBlockExpr(TypedAstType, TypedBlock), // { a += 1; }
    NumAddExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 1 + 2
    NumSubExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 2 - 1
    NumMulExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 2 * 2
    NumLessThanOrEqualExpr(TypedAstType, Box<TypedExpr>, Box<TypedExpr>), // 1 <= 2
}

impl TypedExpr {
    pub fn get_typed_ast_type(&self) -> TypedAstType {
        match self {
            TypedExpr::CallExpr(typed_ast_type, _) => typed_ast_type.clone(),
            TypedExpr::NumExpr(typed_ast_type, _) => typed_ast_type.clone(),
            TypedExpr::BoolExpr(typed_ast_type, _) => typed_ast_type.clone(),
            TypedExpr::NumIdentExpr(typed_ast_type, _) => typed_ast_type.clone(),
            TypedExpr::BoolIdentExpr(typed_ast_type, _) => typed_ast_type.clone(),
            TypedExpr::NumBlockExpr(typed_ast_type, _)
            | TypedExpr::BoolBlockExpr(typed_ast_type, _)
            | TypedExpr::VoidBlockExpr(typed_ast_type, _)
            => typed_ast_type.clone(),
            TypedExpr::NumAddExpr(typed_ast_type, ..)
            | TypedExpr::NumSubExpr(typed_ast_type, ..)
            | TypedExpr::NumMulExpr(typed_ast_type, ..)
            | TypedExpr::NumLessThanOrEqualExpr(typed_ast_type, ..) => typed_ast_type.clone(),
        }
    }

    #[allow(dead_code)]
    pub fn num_expr_new(num: i64, raw_num_string: String) -> TypedExpr {
        TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(num, raw_num_string))
    }

    #[allow(dead_code)]
    pub fn bool_expr_new(bool: bool) -> TypedExpr {
        TypedExpr::BoolExpr(TypedAstType::Bool, TypedBool::new(bool))
    }

    #[allow(dead_code)]
    pub fn num_add_new(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        TypedExpr::NumAddExpr(TypedAstType::Number, Box::new(left), Box::new(right))
    }

    #[allow(dead_code)]
    pub fn num_sub_new(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        TypedExpr::NumSubExpr(TypedAstType::Number, Box::new(left), Box::new(right))
    }

    #[allow(dead_code)]
    pub fn num_mul_new(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        TypedExpr::NumMulExpr(TypedAstType::Number, Box::new(left), Box::new(right))
    }

    #[allow(dead_code)]
    pub fn num_block_new(stmts: Vec<TypedStmt>) -> TypedExpr {
        TypedExpr::NumBlockExpr(TypedAstType::Number, TypedBlock::new(stmts))
    }

    #[allow(dead_code)]
    pub fn bool_block_new(stmts: Vec<TypedStmt>) -> TypedExpr {
        TypedExpr::BoolBlockExpr(TypedAstType::Bool, TypedBlock::new(stmts))
    }

    #[allow(dead_code)]
    pub fn void_block_new(stmts: Vec<TypedStmt>) -> TypedExpr {
        TypedExpr::VoidBlockExpr(TypedAstType::Void, TypedBlock::new(stmts))
    }

    #[allow(dead_code)]
    pub fn call_expr_new(typed_ast_type: TypedAstType, func_name: TypedIdent, args: Vec<TypedExpr>) -> TypedExpr {
        TypedExpr::CallExpr(typed_ast_type, TypedCallExpr::new(func_name, args))
    }

    #[allow(dead_code)]
    pub fn num_ident_new(ident: TypedIdent) -> TypedExpr {
        TypedExpr::NumIdentExpr(TypedAstType::Number, ident)
    }

    #[allow(dead_code)]
    pub fn bool_ident_new(ident: TypedIdent) -> TypedExpr {
        TypedExpr::BoolIdentExpr(TypedAstType::Bool, ident)
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedVariableDeclaration {
    name: TypedIdent,
    type_name: Option<TypeFlag>,
    value: Option<TypedExpr>,
}

impl TypedVariableDeclaration {
    pub fn new(
        name: TypedIdent,
        type_name: Option<TypeFlag>,
        value: Option<TypedExpr>,
    ) -> TypedVariableDeclaration {
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

#[derive(Debug, PartialEq, Clone)]
pub struct TypedFuncArg {
    name: TypedIdent,
    arg_type: TypeFlag,
}

impl TypedFuncArg {
    pub fn new(name: TypedIdent, arg_type: TypeFlag) -> TypedFuncArg {
        TypedFuncArg { name, arg_type }
    }

    pub fn get_name(&self) -> TypedIdent {
        self.name.clone()
    }

    pub fn get_arg_type(&self) -> TypeFlag {
        self.arg_type.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedReturnStmt {
    expr: TypedExpr,
}

impl TypedReturnStmt {
    pub fn new(expr: TypedExpr) -> TypedReturnStmt {
        TypedReturnStmt { expr }
    }

    pub fn get_expr(&self) -> TypedExpr {
        self.expr.clone()
    }

    pub fn get_return_type(&self) -> TypedAstType {
        self.expr.get_typed_ast_type()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunc {
    name: TypedIdent,
    args: Vec<TypedFuncArg>,
    stmts: Vec<TypedStmt>,
    return_ast_type: TypedAstType,
}

impl TypedFunc {
    pub fn new(
        name: TypedIdent,
        args: Vec<TypedFuncArg>,
        stmts: Vec<TypedStmt>,
        return_ast_type: TypedAstType,
    ) -> TypedFunc {
        TypedFunc {
            name,
            args,
            stmts,
            return_ast_type,
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

    pub fn get_return_ast_type(&self) -> TypedAstType {self.return_ast_type.clone()}

}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedCanElseStmt {
    BlockBox(TypedBlockBox),
    IfStmt(TypedIfStmt),
}

impl TypedCanElseStmt {

    #[allow(dead_code)]
    pub fn block_box_new(stmts: Vec<TypedStmt>, return_ast_type: TypedAstType) -> TypedCanElseStmt {
        TypedCanElseStmt::BlockBox(TypedBlockBox::new(stmts, return_ast_type))
    }

    #[allow(dead_code)]
    pub fn if_stmt_new(condition_expr: TypedExpr, then_stmt: TypedBlockBox, else_stmt: Option<TypedCanElseStmt>, return_ast_type: TypedAstType) -> TypedCanElseStmt {
        TypedCanElseStmt::IfStmt(TypedIfStmt::new(condition_expr, then_stmt, else_stmt, return_ast_type))
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedIfStmt {
    condition_expr: TypedExpr,
    then_stmt: TypedBlockBox,
    else_stmt: Option<Box<TypedCanElseStmt>>,
    return_ast_type: TypedAstType,
}

impl TypedIfStmt {
    pub fn new(condition_expr: TypedExpr, then_stmt: TypedBlockBox, else_stmt: Option<TypedCanElseStmt>, return_ast_type: TypedAstType) -> TypedIfStmt {
        let else_stmt = match else_stmt {
            Some(can_else_stmt) => Some(Box::new(can_else_stmt)),
            None => None,
        };

        TypedIfStmt {
            condition_expr,
            then_stmt,
            else_stmt,
            return_ast_type
        }

    }

    pub fn get_condition_expr(&self) -> TypedExpr {
        self.condition_expr.clone()
    }

    pub fn get_then_stmt(&self) -> TypedBlockBox {
        self.then_stmt.clone()
    }

    pub fn get_else_stmt(&self) -> Option<&Box<TypedCanElseStmt>> {
        match &self.else_stmt {
            Some(box_typed_can_else_stmt) => Some(box_typed_can_else_stmt),
            None => None
        }
    }

    pub fn get_return_ast_type(&self) -> TypedAstType {
        self.return_ast_type.clone()
    }


}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStmt {
    VariableDeclaration(TypedVariableDeclaration),
    ExprStmt(TypedExpr),
    Func(TypedFunc),
    ReturnStmt(TypedReturnStmt),
    IfStmt(TypedIfStmt),
}

impl TypedStmt {

    #[allow(dead_code)]
    pub fn var_new(
        name: TypedIdent,
        type_name: Option<TypeFlag>,
        value: Option<TypedExpr>,
    ) -> TypedStmt {
        TypedStmt::VariableDeclaration(
            TypedVariableDeclaration::new(name, type_name, value)
        )
    }

    #[allow(dead_code)]
    pub fn expr_new(expr: TypedExpr) -> TypedStmt {
        TypedStmt::ExprStmt(
            expr
        )
    }

    #[allow(dead_code)]
    pub fn func_new(
        name: TypedIdent,
        args: Vec<TypedFuncArg>,
        stmts: Vec<TypedStmt>,
        return_ast_type: TypedAstType,
    ) -> TypedStmt {
        TypedStmt::Func(
            TypedFunc::new(name, args, stmts, return_ast_type)
        )
    }

    #[allow(dead_code)]
    pub fn return_new(expr: TypedExpr) -> TypedStmt {
        TypedStmt::ReturnStmt(TypedReturnStmt::new(expr))
    }

    #[allow(dead_code)]
    pub fn if_stmt_new(condition_expr: TypedExpr, then_stmt: TypedBlockBox, else_stmt: Option<TypedCanElseStmt>, return_ast_type: TypedAstType) -> TypedStmt {
        TypedStmt::IfStmt(TypedIfStmt::new(condition_expr, then_stmt, else_stmt, return_ast_type))
    }

}
