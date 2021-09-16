use crate::parser::ast::{Ident, Types};
use crate::type_parser::check_and_inference::type_check_and_inference_struct::TypeCheckAndInference;
use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedIdent};

impl TypeCheckAndInference {
    pub fn convert_ident_to_typed_ident(&self, ident: Ident) -> TypedIdent {
        TypedIdent::new(ident.get_name())
    }

    pub fn convert_type_to_typed_type(&self, type_flag: Types) -> TypeFlag {
        match type_flag {
            Types::NumberType => TypeFlag::NumberType,
        }
    }

    pub fn convert_type_to_typed_ast_type(&self, type_flag: Types) -> TypedAstType {
        match type_flag {
            Types::NumberType => TypedAstType::Number,
            _ => TypedAstType::Func(
                vec![TypedAstType::Number],
                Some(Box::new(TypedAstType::Number)),
            ), // TODO: 適当に書いた、後で直す
        }
    }
}
