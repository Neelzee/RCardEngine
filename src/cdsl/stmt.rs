use super::expr::{Type, Value, ExprAst};

pub enum Statement {
    VarDecleration(String, Type, Option<Value>),
    FunDecleration(String, Vec<Type>, Type, Vec<Statement>),
    VarAssignment(String, Value),
    If(ExprAst, Vec<Statement>),
    IfElse(ExprAst, Vec<Statement>, Vec<Statement>),
    // int i = 0; i <= limit; i++
    ForLoop(ExprAst, ExprAst, ExprAst, Vec<Statement>),
    WhileLoop(ExprAst, Vec<Statement>),
    Increment(ExprAst),
    Break,
    Continue,
    Switch(ExprAst, Vec<(ExprAst, Vec<Statement>)>)
}

