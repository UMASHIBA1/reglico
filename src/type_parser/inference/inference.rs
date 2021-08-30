use crate::parser::ast::{Stmt, Expr, VariableDeclaration, Ident, Types, Opcode, Func, ReturnStmt, CallExpr};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedNumber, TypedAstType, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};
use std::collections::HashMap;

// inference したいところ ->
pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeInference::inference(stmts, None)
}

struct TypeInference{
    type_env: HashMap<TypedIdent, TypedAstType>
}

impl TypeInference {
    pub fn inference(stmts: Vec<Stmt>, type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> Vec<TypedStmt>{
        let mut type_inference = TypeInference::new(type_env);
        let mut typed_stmts = vec![];
        for stmt in stmts {
            typed_stmts.push(type_inference.inference_a_stmt(stmt));
        };
        typed_stmts
    }

    fn new(type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> TypeInference {
        TypeInference {
            type_env: match type_env {
                Some(env) => env.clone(),
                None => HashMap::new(),
            }
        }
    }

    fn inference_a_stmt(&mut self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => TypedStmt::VariableDeclaration(self.inference_var_declaration(var_decl)),
            Stmt::ExprStmt(expr_stmt) => TypedStmt::ExprStmt(self.inference_expr(expr_stmt.get_expr())),
            Stmt::Func(func) => TypedStmt::Func(self.inference_func(func)),
        }
    }

    fn inference_var_declaration(&mut self, var_decl: VariableDeclaration) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());
        let type_name = match var_decl.get_type_name() {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag)),
            None => None
        };
        let value = match var_decl.get_value() {
            Some(expr) => {
                let expr_value = self.inference_expr(expr);
                &self.type_env.insert(name.clone(), expr_value.get_typed_ast_type());
                Some(expr_value)
            },
            None => None
        };

        TypedVariableDeclaration::new(
            name,
            type_name,
            value
        )
    }


    fn inference_expr(&self, expr: Expr) -> TypedExpr {
        match expr {
            Expr::Num(num) => TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(num.get_num())),
            Expr::Op(operation) => match operation.get_operation() {
                // TODO: 後々String等の+演算等も出てくると思うのでここをStrAddExprとかFloatAddExprとか追加する
                (l, Opcode::Add, r) => TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(self.inference_expr(l)),
                    Box::new(self.inference_expr(r))
                ),
            },
            Expr::Call(call_expr) => self.inference_call(call_expr),
            Expr::Ident(ident) => self.inference_ident(ident),
        }
    }

    fn inference_call(&self, call_expr: CallExpr) -> TypedExpr {
        let func_name = call_expr.get_func_name();
        let args = call_expr.get_args();

        let typed_func_name = self.convert_ident_to_typed_ident(func_name);

        let typed_ast_type = self.type_env.get(&typed_func_name);

        let mut typed_args = vec![];
        for arg in args {
            typed_args.push(self.inference_expr(arg));
        }



        let func_return_ast_type = match typed_ast_type {
            Some(typed_ast_type) => {
                match typed_ast_type {
                    TypedAstType::Func(_, return_type) => {
                        match return_type {
                            Some(return_type) => return_type,
                            None => panic!("in type_parsing call, the calling func is not valid"),
                        }
                    },
                    _ => panic!("parsing call is not call func: {:?}", typed_ast_type)
                }
            },
            None => panic!("parsing call is not defined value{:?}", &typed_func_name)
        };

        let typed_call_expr = TypedCallExpr::new(
            typed_func_name,
            typed_args
        );

        TypedExpr::CallExpr(*func_return_ast_type.clone(), typed_call_expr)
    }

    fn inference_func(&mut self, func: Func) -> TypedFunc {
        let name = self.convert_ident_to_typed_ident(func.get_name());
        let args = func.get_func_args();
        let stmts =  func.get_stmts();
        let return_stmt = func.get_return_stmt();
        let mut arg_typed_ast_type = vec![];

        // NOTE: inference args type
        let mut typed_args = vec![];
        for arg in args {
            let arg_type = arg.get_arg_type();
            let typed_arg_type = self.convert_type_to_typed_type(arg_type.clone());

            arg_typed_ast_type.push(
                self.convert_type_to_typed_ast_type(arg_type)
            );

            typed_args.push(
                TypedFuncArg::new(
                    self.convert_ident_to_typed_ident(arg.get_name()),
                    typed_arg_type,
                )
            );
        };

        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            self.type_env.insert(typed_arg.get_name(), arg_typed_ast_type.get(i).unwrap().clone());
        };
        println!("{:?}", self.type_env);

        // inference return type
        let typed_return_stmt =  self.convert_return_stmt_to_typed_return_stmt(return_stmt);
        let return_typed_ast_type = {
            match &typed_return_stmt {
                Some(typed_return_stmt) => {
                    Some(Box::new(typed_return_stmt.get_expr().get_typed_ast_type()))
                },
                None => Some(Box::new(TypedAstType::Void)),
            }
        };

        self.type_env.insert(name.clone(), TypedAstType::Func(arg_typed_ast_type, return_typed_ast_type));

        let func_stmts = TypeInference::inference(stmts, Some(&self.type_env));

        // NOTE: remove arg env from type_env
        for typed_arg in &typed_args {
            self.type_env.remove(&typed_arg.get_name());
        };

        TypedFunc::new(
            name,
            typed_args,
            func_stmts,
            typed_return_stmt
        )

    }

    fn inference_ident(&self, ident: Ident) -> TypedExpr {
        let typed_ident  = self.convert_ident_to_typed_ident(ident);
        let typed_ast_type = self.type_env.get(&typed_ident);
        match typed_ast_type {
            Some(typed_ast_type) => {
                // TODO: 今後Num意外にもたくさんIdentに格納されるものが増えるはずなので増えた際はここで判定処理をする
                TypedExpr::NumIdentExpr(typed_ast_type.clone(), typed_ident)
            },
            None => {
                panic!("parsing ident is not defined value${:?}", typed_ident);
            }
        }
    }

    fn convert_return_stmt_to_typed_return_stmt(&self, return_stmt: Option<ReturnStmt>) -> Option<TypedReturnStmt> {
        match return_stmt {
            Some(return_stmt) => {
                  Some(TypedReturnStmt::new(
                      self.inference_expr(return_stmt.get_expr()),
                  ))
            },
            None => None
        }

    }

    fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType
        }
    }

    fn convert_type_to_typed_ast_type(&self, type_flag: Types) -> TypedAstType {
        match type_flag {
            Types::NumberType => TypedAstType::Number,
            _ => TypedAstType::Func(vec![TypedAstType::Number],Some(Box::new(TypedAstType::Number))) // TODO: 適当に書いた、後で直す
        }
    }

}


#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Ident, Types, Expr, Opcode, FuncArg, ReturnStmt};
    use crate::type_parser::inference::inference::inference;
    use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedIdent, TypeFlag, TypedExpr, TypedAstType, TypedNumber, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};

    #[test]
    fn test_inference_var_declaration(){
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10))
        )];

        let typed_stmts = inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new("tmp1".to_string()),
                    Some(TypeFlag::NumberType),
                    Some(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(10)))
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_add_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Add,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumAddExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_ident_expr_stmt(){
        let stmts = vec![
            Stmt::var_new(
                Ident::new("tmp1".to_string()),
                Some(Types::NumberType),
                Some(Expr::num_new(10))
            ),
            Stmt::expr_new(Expr::ident_new(Ident::new("tmp1".to_string())))
        ];

        let typed_stmts = inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::VariableDeclaration(
                TypedVariableDeclaration::new(
                    TypedIdent::new("tmp1".to_string()),
                    Some(TypeFlag::NumberType),
                    Some(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(10)))
                )
            ),
            TypedStmt::ExprStmt(
                TypedExpr::NumIdentExpr(
                    TypedAstType::Number,
                    TypedIdent::new("tmp1".to_string()),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_func(){
        let stmts = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![],
                Some(
                    ReturnStmt::new(
                        Expr::op_new(
                            Expr::ident_new(Ident::new("a".to_string())),
                            Opcode::Add,
                            Expr::ident_new(Ident::new("b".to_string())),
                        )
                    )
                )
            ),
        ];

        let typed_stmts = inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),

                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_call(){
        let stmts = vec![
            Stmt::func_new(
                Ident::new("add".to_string()),
                vec![
                    FuncArg::new(Ident::new("a".to_string()), Types::NumberType),
                    FuncArg::new(Ident::new("b".to_string()), Types::NumberType),
                ],
                vec![],
                Some(
                    ReturnStmt::new(
                        Expr::op_new(
                            Expr::ident_new(Ident::new("a".to_string())),
                            Opcode::Add,
                            Expr::ident_new(Ident::new("b".to_string())),
                        )
                    )
                )
            ),
            Stmt::expr_new(
                Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)]
                )
            )
        ];

        let typed_stmts = inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::Func(
                TypedFunc::new(
                    TypedIdent::new("add".to_string()),

                    vec![
                        TypedFuncArg::new(TypedIdent::new("a".to_string()), TypeFlag::NumberType),
                        TypedFuncArg::new(TypedIdent::new("b".to_string()), TypeFlag::NumberType),
                    ],
                    vec![],
                    Some(
                        TypedReturnStmt::new(
                            TypedExpr::NumAddExpr(
                                TypedAstType::Number,
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("a".to_string()))),
                                Box::new(TypedExpr::NumIdentExpr(TypedAstType::Number, TypedIdent::new("b".to_string()))),
                            )
                        )
                    )
                )
            ),
            TypedStmt::ExprStmt(
                TypedExpr::CallExpr(
                    TypedAstType::Number,
                    TypedCallExpr::new(
                        TypedIdent::new("add".to_string()),
                        vec![
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1)),
                            TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2)),
                        ]
                    )
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }


}
