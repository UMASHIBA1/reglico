use crate::type_parser::typed_ast::{TypedStmt, TypedVariableDeclaration, TypedExpr, TypeFlag};

pub fn to_rust(typed_stmt: TypedStmt) -> String {
    match typed_stmt {
        TypedStmt::VariableDeclaration(var_decl) => var_decl_to_rust(var_decl),
        _ => panic!("tmp panic"),
    }
}

fn var_decl_to_rust(var_decl: TypedVariableDeclaration) -> String {
    let name = var_decl.get_name().get_name();
    let type_name = var_decl.get_type_name();
    let value = var_decl.get_value();

    // TODO: 所有権について後でどうするかちゃんと考える
    match value {
        Some(typed_expr) => {
            match type_name {
                Some(type_flag) => format!("let {}: {} = {};", name, type_flag_to_rust(type_flag), expr_to_rust(typed_expr)),
                None => format!("let {} = {};", name, expr_to_rust(typed_expr)),
            }
        },
        None => format!("let mut {};", name),
    }
}

fn type_flag_to_rust(type_flag: TypeFlag) -> String {
    match type_flag {
        TypeFlag::NumberType => "i32".to_string()
    }
}


// TODO: 後でちゃんとしたもの作る
fn expr_to_rust(typed_expr: TypedExpr) -> String {
    "10".to_string()
}

