use crate::parser::ast::{Stmt, Expr, VariableDeclaration, Ident, Types, Opcode};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedNumber, TypedAstType};
use std::collections::HashMap;

// inference したいところ ->
pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeInference::inference(stmts)
}

struct TypeInference{
    type_env: HashMap<TypedIdent, TypedAstType>
}

impl TypeInference {
    pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt>{
        let mut type_inference = TypeInference::new();
        let mut typed_stmts = vec![];
        for stmt in stmts {
            typed_stmts.push(type_inference.inference_a_stmt(stmt));
        };
        typed_stmts
    }

    fn new() -> TypeInference {
        TypeInference {
            type_env: HashMap::new()
        }
    }

    fn inference_a_stmt(&mut self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => TypedStmt::VariableDeclaration(self.inference_var_declaration(var_decl)),
            Stmt::ExprStmt(expr_stmt) => TypedStmt::ExprStmt(self.inference_expr(expr_stmt.get_expr())),
            _ => TypedStmt::VariableDeclaration(TypedVariableDeclaration::new(TypedIdent::new("tmp".to_string()), None, None)) // TODO: コンパイル通すため一時的にこうしてる、直す
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
            Expr::Ident(ident) => self.inference_ident(ident),
            _ => TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0)) // TODO: 一旦コンパイル通すためこうしてる、ちゃんと作る
        }
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

    fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType
        }
    }

}


#[cfg(test)]
mod tests {
    use crate::parser::ast::{Stmt, Ident, Types, Expr, Opcode};
    use crate::type_parser::inference::inference::inference;
    use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedIdent, TypeFlag, TypedExpr, TypedAstType, TypedNumber};

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

}