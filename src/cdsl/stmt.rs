use super::expr::{Type, Value, Expr};

pub enum Statement {
    VarDecleration(String, Type, Option<Value>),
    FunDecleration(String, Vec<Type>, Type, Vec<Statement>),
    VarAssignment(String, Value),
    If(Expr, Vec<Statement>),
    IfElse(Expr, Vec<Statement>, Vec<Statement>),
    // int i = 0; i <= limit; i++
    ForLoop(Expr, Expr, Expr, Vec<Statement>),
    WhileLoop(Expr, Vec<Statement>),
    Increment(Expr),
    Break,
    Continue,
    Switch(Expr, Vec<(Expr, Vec<Statement>)>)
}

