use super::{
    expr::Expr,
    interface::{FnDecl, Sort, Value, Variable},
};
use core::hash::Hash;
use std::collections::HashMap;

pub trait UnitType: Hash + Eq + Ord + Clone {}

pub struct Algebra<T>
where
    T: UnitType,
{
    values: HashMap<Variable, Value<T>>,

    fn_decls: HashMap<FnDecl, Box<dyn Fn(Vec<Value<T>>) -> Value<T>>>,
}

impl<T> Algebra<T>
where
    T: UnitType,
{
    pub fn new() -> Self {
        Algebra {
            values: HashMap::new(),
            fn_decls: HashMap::new(),
        }
    }

    fn alpha(&self, var: &Variable) -> Option<Value<T>> {
        self.values.get(var).cloned()
    }

    pub fn assign(&mut self, var: Variable, val: Value<T>) {
        self.values.insert(var, val);
    }

    pub fn add_fn(&mut self, fn_decl: FnDecl, fn_expr: Box<dyn Fn(Vec<Value<T>>) -> Value<T>>) {
        self.fn_decls.insert(fn_decl, fn_expr);
    }

    pub fn eval(&self, expr: Expr) -> Option<Value<T>> {
        match expr {
            Expr::Var(var) => self.alpha(&var),

            Expr::Fun(fn_decl, args) => {
                if let Some(f) = self.fn_decls.get(&fn_decl)
                    && self.parse_args(args.clone(), fn_decl.arg_sorts())
                {
                    Some(f(args
                        .into_iter()
                        .filter_map(|e| self.alpha(&e))
                        .collect::<Vec<_>>()))
                } else {
                    None
                }
            }
        }
    }

    // TODO: Add error handling
    fn parse_args(&self, args: Vec<Variable>, sorts: Vec<Sort>) -> bool {
        args.into_iter().zip(sorts).all(|(a, b)| a.get_sort() == b)
    }
}
