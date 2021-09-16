use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedIdent};

impl ToRust {
    pub fn type_flag_to_rust(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "f32".to_string(),
        }
    }

    pub fn typed_ast_type_to_rust(&self, typed_ast_type: TypedAstType) -> String {
        match typed_ast_type {
            TypedAstType::Number => "f32".to_string(),
            TypedAstType::Void => "()".to_string(),
            _ => "()".to_string(), // TODO: Funcの型生成するの面倒くさいので後回しにしてます、あとでやりましょう！
        }
    }

    pub fn is_exist_ident(&self, ident: &TypedIdent) -> bool {
        match self.var_env.get(ident) {
            Some(option_type_expr) => match option_type_expr {
                Some(_) => true,
                None => false,
            },
            None => false,
        }
    }
}
