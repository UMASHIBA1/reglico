use crate::to_ts_rust::to_ts::to_ts::ToTs;
use crate::type_parser::typed_ast::{TypeFlag, TypedAstType, TypedIdent, TypedBlockBox, TypedStmt};

impl ToTs {
    pub fn type_flag_to_ts(&self, type_flag: TypeFlag) -> String {
        match type_flag {
            TypeFlag::NumberType => "number".to_string(),
            TypeFlag::BoolType => "boolean".to_string(),
        }
    }

    pub fn typed_ast_type_to_ts(&self, typed_ast_type: TypedAstType) -> String {
        match typed_ast_type {
            TypedAstType::Number => "number".to_string(),
            TypedAstType::Bool => "boolean".to_string(),
            TypedAstType::Void => "void".to_string(),
            _ => "void".to_string(), // TODO: Funcの型生成する
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

    pub fn block_box_to_ts(&self, block_box: TypedBlockBox) -> String {
        let block_box_var_env = self.var_env.clone();
        let mut stmts = block_box.get_stmts();
        match block_box.get_return_stmt() {
            Some(return_stmt) => {
                stmts.push(TypedStmt::ReturnStmt(return_stmt))
            },
            None => {}
        }

        let str_stmts = ToTs::to_ts(stmts, Some(block_box_var_env));

        format!("{{{}}}", str_stmts)

    }

}
