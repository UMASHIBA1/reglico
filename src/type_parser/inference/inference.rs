use crate::parser::ast::{Stmt, Expr, VariableDeclaration, Ident, Types};
use crate::type_parser::typed_ast::{TypedStmt, TypedIdent, TypedVariableDeclaration, TypedExpr, TypeFlag, TypedNumber, TypedAstType};
use std::collections::HashMap;

// inference したいところ ->
pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt> {
    TypeInference::inference(stmts)
}

struct TypeInference {
    type_env: HashMap<TypedIdent, TypedAstType>
}

impl TypeInference {
    pub fn inference(stmts: Vec<Stmt>) -> Vec<TypedStmt>{
        let type_inference = TypeInference::new();
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

    fn inference_a_stmt(&self, stmt: Stmt) -> TypedStmt {
        match stmt {
            Stmt::VariableDeclaration(var_decl) => TypedStmt::VariableDeclaration(self.inference_var_declaration(var_decl)),
            _ => TypedStmt::VariableDeclaration(TypedVariableDeclaration::new(TypedIdent::new("tmp".to_string()), None, None)) // TODO: コンパイル通すため一時的にこうしてる、直す
        }
    }

    fn inference_var_declaration(&self, var_decl: VariableDeclaration) -> TypedVariableDeclaration {
        let name = self.convert_ident_to_typed_ident(var_decl.get_var_name());
        let type_name = match var_decl.get_type_name() {
            Some(type_flag) => Some(self.convert_type_to_typed_type(type_flag)),
            None => None
        };
        let value = match var_decl.get_value() {
            Some(expr) => Some(self.inference_expr(expr)),
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
            _ => TypedExpr::NumExpr(TypedAstType::Number, TypedNumber::new(0)) // TODO: 一旦コンパイル通すためこうしてる、ちゃんと作る
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
    #[test]
    fn test_inference_var_declaration(){
        
    }
}