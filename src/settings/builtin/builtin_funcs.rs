use crate::type_parser::typed_ast::{TypedAstType};
use crate::to_ts_rust::common_struct::CanAssignObj;
use crate::settings::builtin::console_log::CONSOLE_LOG;
use once_cell::sync::Lazy;
use crate::settings::builtin::performance_now::PERFORMANCE_NOW;

pub struct BuiltInFunc<'a> {
    name: &'a str,
    rust_func_def: &'a str,
    ts_func_def: &'a str,
    typed_ast_type: TypedAstType,
    can_assign_obj: CanAssignObj,
}

impl BuiltInFunc<'_> {
    pub fn new<'a>(name: &'a str, rust_func_def: &'a str, ts_func_def: &'a str, typed_ast_type: TypedAstType, can_assign_obj: CanAssignObj) -> BuiltInFunc<'a> {
        BuiltInFunc {
            name,
            rust_func_def,
            ts_func_def,
            typed_ast_type,
            can_assign_obj
        }
    }

    pub fn get_name(&self) -> &str {
        self.name
    }

    pub fn get_rust_func_def(&self) -> &str {
        self.rust_func_def
    }

    pub fn get_ts_func_def(&self) -> &str {
        self.ts_func_def
    }

    pub fn get_typed_ast_type(&self) -> TypedAstType {
        self.typed_ast_type.clone()
    }

    pub fn get_can_assign_obj(&self) -> CanAssignObj {
        self.can_assign_obj.clone()
    }
}


// TODO: union型ができたらprint関数でnumber以外も受け取れるようにする
pub const BUILTIN_FUNCS: [Lazy<BuiltInFunc>; 2] = [
    CONSOLE_LOG,
    PERFORMANCE_NOW,
];
