use super::expr::{Type, Value, ExprAst};

pub enum Statement {
    VarDeclaration(String, Type, Option<Value>),
    FunDeclaration(String, Vec<Type>, Type, Vec<Statement>),
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

