use crate::to_ts_rust::to_rust::to_rust::ToRust;
use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedIdent, TypedBlockBox};

impl ToRust {
    pub fn type_flag_to_rust(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "i64".to_string(),
            TypeFlag::BoolType => "bool".to_string(),
        }
    }

    pub fn typed_ast_type_to_rust(&self, typed_ast_type: TypedAstType) -> String {
        match typed_ast_type {
            TypedAstType::Number => "i64".to_string(),
            TypedAstType::Bool => "bool".to_string(),
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

    pub fn block_box_to_rust(&self, block_box: TypedBlockBox) -> String {
        let block_box_var_env = self.var_env.clone();
        let mut stmts = block_box.get_stmts();

        let str_stmts = ToRust::to_rust(stmts, Some(block_box_var_env));

        format!("{{{}}}", str_stmts)

    }

}
