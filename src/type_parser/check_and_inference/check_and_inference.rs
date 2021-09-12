use crate::parser::ast::{Stmt, Expr, VariableDeclaration, Ident, Types, Opcode, Func, ReturnStmt, CallExpr, Operation};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedNumber, TypedAstType, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};
use std::collections::HashMap;

pub fn check_and_inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeCheckAndInference::check_and_inference(stmts, None)
}

struct TypeCheckAndInference {
    type_env: HashMap<TypedIdent, TypedAstType>
}

impl TypeCheckAndInference {
    pub fn check_and_inference(stmts: Vec<Stmt>, type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> Vec<TypedStmt>{
        let mut type_inference = TypeCheckAndInference::new(type_env);
        let mut typed_stmts = vec![];
        for stmt in stmts {
            typed_stmts.push(type_inference.check_and_inference_a_stmt(stmt));
        };
        typed_stmts
    }

    fn new(type_env: Option<&HashMap<TypedIdent, TypedAstType>>) -> TypeCheckAndInference {
        TypeCheckAndInference {
            type_env: match type_env {
                Some(env) => env.clone(),
                None => HashMap::new(),
            }
        }
    }

    fn check_and_inference_a_stmt(&mut self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => TypedStmt::VariableDeclaration(self.check_and_inference_var_declaration(var_decl)),
            Stmt::ExprStmt(expr_stmt) => TypedStmt::ExprStmt(self.check_and_inference_expr(expr_stmt.get_expr())),
            Stmt::Func(func) => TypedStmt::Func(self.check_and_inference_func(func)),
            Stmt::ReturnStmt(return_stmt) => TypedStmt::ReturnStmt(self.convert_return_stmt_to_typed_return_stmt(return_stmt)),
        }
    }

    fn check_and_inference_var_declaration(&mut self, var_decl: VariableDeclaration) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());
        let type_name = match var_decl.get_type_name() {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag)),
            None => None
        };
        let value = match var_decl.get_value() {
            Some(expr) => {
                let expr_value = self.check_and_inference_expr(expr);
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


    fn check_and_inference_expr(&self, expr: Expr) -> TypedExpr {
        match expr {
            Expr::Num(num) => TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(num.get_num())),
            Expr::Op(operation) => self.check_and_inference_op(operation),
            Expr::Call(call_expr) => self.check_and_inference_call(call_expr),
            Expr::Ident(ident) => self.check_and_inference_ident(ident),
        }
    }

    fn check_and_inference_op(&self, operation: Operation) -> TypedExpr {
        match operation.get_operation() {
            // TODO: 後々String等の+演算等も出てくると思うのでここをStrAddExprとかFloatAddExprとか追加する
            (l, Opcode::Add, r) => TypedExpr::NumAddExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r))
            ),
            (l, Opcode::Sub, r) => TypedExpr::NumSubExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r))
            ),
            (l, Opcode::Mul, r) => TypedExpr::NumMulExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r))
            ),
            (l, Opcode::Div, r) => TypedExpr::NumDivExpr(
                TypedAstType::Number,
                Box::new(self.check_and_inference_expr(l)),
                Box::new(self.check_and_inference_expr(r)),
            )
        }
    }

    fn check_and_inference_call(&self, call_expr: CallExpr) -> TypedExpr {
        let func_name = call_expr.get_func_name();
        let args = call_expr.get_args();

        let typed_func_name = self.convert_ident_to_typed_ident(func_name);

        let typed_ast_type = self.type_env.get(&typed_func_name);

        let mut typed_args = vec![];
        for arg in args {
            typed_args.push(self.check_and_inference_expr(arg));
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

    fn check_and_inference_func(&mut self, func: Func) -> TypedFunc {
        let name = self.convert_ident_to_typed_ident(func.get_name());
        let args = func.get_func_args();
        let stmts =  func.get_stmts();
        let mut arg_typed_ast_type = vec![];

        // NOTE: check_and_inference args type
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

        let mut func_type_env = self.type_env.clone();
        // NOTE: add arg type to type_env
        for (i, typed_arg) in typed_args.iter().enumerate() {
            func_type_env.insert(typed_arg.get_name(), arg_typed_ast_type.get(i).unwrap().clone());
        };

        let mut func_stmts = TypeCheckAndInference::check_and_inference(stmts, Some(&func_type_env));
        let mut typed_return_stmt: Option<TypedReturnStmt> = None;
        let mut return_stmt_index: Option<usize> = None;
            for (i, stmt) in func_stmts.iter().enumerate() {
                match stmt {
                    TypedStmt::ReturnStmt(_) => {
                        return_stmt_index = Some(i);
                    },
                    _ => {}
                }
            }
        match return_stmt_index {
            Some(i) => {
                let will_typed_return_stmt = func_stmts.remove(i);
                match will_typed_return_stmt {
                    TypedStmt::ReturnStmt(return_stmt) => {
                        typed_return_stmt = Some(return_stmt);
                    }
                    _ => {},
                };
            },
            _ => {}
        }

        let return_typed_ast_type = {
            match &typed_return_stmt {
                Some(typed_return_stmt) => {
                    Some(Box::new(typed_return_stmt.get_expr().get_typed_ast_type()))
                },
                None => Some(Box::new(TypedAstType::Void)),
            }
        };

        self.type_env.insert(name.clone(), TypedAstType::Func(arg_typed_ast_type, return_typed_ast_type));

        TypedFunc::new(
            name,
            typed_args,
            func_stmts,
            typed_return_stmt
        )

    }

    fn check_and_inference_ident(&self, ident: Ident) -> TypedExpr {
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

    fn convert_return_stmt_to_typed_return_stmt(&self, return_stmt: ReturnStmt) -> TypedReturnStmt {
        TypedReturnStmt::new(
            self.check_and_inference_expr(return_stmt.get_expr()),
        )
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
    use crate::type_parser::check_and_inference::check_and_inference::check_and_inference;
    use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedIdent, TypeFlag, TypedExpr, TypedAstType, TypedNumber, TypedFunc, TypedFuncArg, TypedReturnStmt, TypedCallExpr};

    #[test]
    fn test_inference_var_declaration(){
        let stmts = vec![Stmt::var_new(
            Ident::new("tmp1".to_string()),
            Some(Types::NumberType),
            Some(Expr::num_new(10))
        )];

        let typed_stmts = check_and_inference(stmts);

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

        let typed_stmts = check_and_inference(stmts);

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
    fn test_inference_num_sub_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Sub,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumSubExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_mul_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Mul,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumMulExpr(
                    TypedAstType::Number,
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(1))),
                    Box::new(TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(2))),
                )
            )
        ];

        assert_eq!(typed_stmts, expected_typed_stmts)
    }

    #[test]
    fn test_inference_num_div_expr_stmt(){
        let stmts = vec![
            Stmt::expr_new(
                Expr::op_new(
                    Expr::num_new(1),
                    Opcode::Div,
                    Expr::num_new(2),
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

        let expected_typed_stmts = vec![
            TypedStmt::ExprStmt(
                TypedExpr::NumDivExpr(
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

        let typed_stmts = check_and_inference(stmts);

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
                vec![Stmt::ReturnStmt(
                    ReturnStmt::new(
                    Expr::op_new(
                        Expr::ident_new(Ident::new("a".to_string())),
                        Opcode::Add,
                        Expr::ident_new(Ident::new("b".to_string())),
                    )
                ))
                ],
            ),
        ];

        let typed_stmts = check_and_inference(stmts);

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
                vec![Stmt::ReturnStmt(
                        ReturnStmt::new(
                            Expr::op_new(
                                Expr::ident_new(Ident::new("a".to_string())),
                                Opcode::Add,
                                Expr::ident_new(Ident::new("b".to_string())),
                            )
                        )
                )],

            ),
            Stmt::expr_new(
                Expr::call_new(
                    Ident::new("add".to_string()),
                    vec![Expr::num_new(1), Expr::num_new(2)]
                )
            )
        ];

        let typed_stmts = check_and_inference(stmts);

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
