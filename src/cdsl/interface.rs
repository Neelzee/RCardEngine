use core::hash::Hash;

use super::algebra::UnitType;

/// Sorts
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sort {
    Int,
    String,
    Float,
    List(Vec<Sort>),
    Card,
    Player,
    Unit,
}

/// Function Declaration
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnDecl {
    /// Function Name
    name: String,
    /// List of argument sorts
    arg_decls: Vec<Sort>,
}

impl FnDecl {
    pub fn new(name: String, arg_decls: Vec<Sort>) -> Self {
        FnDecl { name, arg_decls }
    }

    pub fn arg_sorts(&self) -> Vec<Sort> {
        self.arg_decls.clone()
    }
}

/// Variables
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    name: String,
    sort: Sort,
}

impl Variable {
    pub fn new(name: String, sort: Sort) -> Self {
        Variable { name, sort }
    }

    pub fn get_sort(&self) -> Sort {
        self.sort.clone()
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value<T>
where
    T: UnitType,
{
    val: T,
}

impl<T> Value<T>
where
    T: UnitType,
{
    pub fn new(val: T) -> Self {
        Value { val }
    }

    pub fn val(&self) -> T {
        self.val.clone()
    }
}
