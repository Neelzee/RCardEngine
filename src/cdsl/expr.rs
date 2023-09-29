use crate::game::player::Player;
use crate::game::card::Card;

pub enum Expr {
    Var(String, Type),
    FunCall(String, Vec<Expr>, Type)
}

#[derive(PartialEq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    List(Box<Type>),
    Card,
    Player
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    List(Vec<Value>),
    Card(Card),
    Player(Player)
}

#[derive(PartialEq)]
pub struct FunDecl {
    pub fun_name: String,
    pub arguments: Vec<Type>,
    pub ret_type: Type
}

#[derive(PartialEq)]
pub struct VarDecl {
    pub var_name: String,
    pub var_type: Type,
    pub value: Value
}



pub struct Scope {
    pub variables: Vec<VarDecl>,
    pub functions: Vec<FunDecl>
}


impl Scope {
    pub fn lookup_variable(&self, expr: &Expr) -> Option<Value> {
        match expr {
            Expr::Var(var_name, var_type) => match self.variables.iter().filter(|vd| &vd.var_name == var_name && &vd.var_type == var_type).collect::<Vec<&VarDecl>>().pop() {
                Some(vd) => Some(vd.value.clone()),
                None => None
            }
            _ => None
        }
    }


    pub fn lookup_function(&self, expr: &Expr) -> Option<FunDecl> {
        match expr {
            Expr::FunCall(fun_name, args, ret_type) => match self.functions.iter().filter(|fd| &fd.fun_name == fun_name && &fd.ret_type == ret_type).collect::<Vec<&FunDecl>>() {
                fs => {
                    todo!()
                }
            }
            _ => None
        }
    }

    /**
     * Takes a union of the scopes
     */
    pub fn expand_scope(self, scope: Scope) -> Scope {
        let mut left_vars = self.variables;
        let right_vars = scope.variables;

        let mut left_funs = self.functions;
        let right_funs = scope.functions;

        for v in right_vars {
            if !left_vars.contains(&v) {
                left_vars.push(v);
            }
        }

        for f in right_funs {
            if !left_funs.contains(&f) {
                left_funs.push(f);
            }
        }

        return Scope {variables: left_vars, functions: left_funs};
    }
}