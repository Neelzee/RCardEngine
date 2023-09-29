use crate::cdsl::expr::{Scope, FunDecl, Type};

pub fn preludeScope() -> Scope {
    Scope {
        variables: Vec::new(),
        functions: vec![
            // Addition of Integer types
            FunDecl {
                fun_name: "_+_".to_string(),
                arguments: vec![Type::Integer, Type::Integer],
                ret_type: Type::Integer
            },

            // Addition of Float Types
            FunDecl {
                fun_name: "_+_".to_string(),
                arguments: vec![Type::Float, Type::Float],
                ret_type: Type::Float
            }
        ]
    }
}