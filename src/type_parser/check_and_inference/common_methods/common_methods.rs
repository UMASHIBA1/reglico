use crate::parser::ast::{Ident, Types, BlockBox};
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedIdent, TypedBlockBox, TypedStmt};

impl TypeCheckAndInference {
    pub fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    pub fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType,
            Types::BoolType => TypeFlag::BoolType,
        }
    }

    pub fn convert_type_to_typed_ast_type(&self, type_flag: Types) -> TypedAstType {
        match type_flag {
            Types::NumberType => TypedAstType::Number,
            Types::BoolType => TypedAstType::Bool,
        }
    }

    pub fn check_and_inference_block_box(&self, block_box: BlockBox) -> TypedBlockBox {
        let stmts = block_box.get_stmts();

        let typed_stmts = TypeCheckAndInference::check_and_inference(stmts, Some(&self.type_env));

        let mut return_ast_type = TypedAstType::Void;
        for typed_stmt in &typed_stmts {
            match typed_stmt {
                TypedStmt::ReturnStmt(return_stmt) => {
                    return_ast_type = return_stmt.get_return_type();
                }
                TypedStmt::IfStmt(if_stmt) => {
                    return_ast_type = if_stmt.get_return_ast_type();
                },
                _ => {}
            };
        };


        TypedBlockBox::new(typed_stmts, return_ast_type)

    }
}
