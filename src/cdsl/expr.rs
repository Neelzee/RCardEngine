use super::interface::{FnDecl, Variable};

/// Expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Var(Variable),
    Fun(FnDecl, Vec<Variable>),
}
