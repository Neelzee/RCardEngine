use crate::game::player::Player;
use crate::game::card::Card;
use std::fmt;

pub enum ExprAst {
    Var(String, Type),
    Add(Box<ExprAst>, Box<ExprAst>),
    Neg(Box<ExprAst>),
    Sub(Box<ExprAst>, Box<ExprAst>),
    Mult(Box<ExprAst>, Box<ExprAst>),
    Div(Box<ExprAst>, Box<ExprAst>),
    FunCall(String, Vec<ExprAst>, Type)
}

impl ExprAst {
    pub fn get_type(&self) -> &Type {
        match self {
            ExprAst::Var(_, t) => t,
            ExprAst::Add(l, _) => l.get_type(), // Assumes the typing is valid
            ExprAst::Neg(e) => e.get_type(),
            ExprAst::Sub(l, _) => l.get_type(), // Assumes the typing is valid
            ExprAst::Mult(l, _) => l.get_type(), // Assumes the typing is valid
            ExprAst::Div(l, _) => l.get_type(), // Assumes the typing is valid
            ExprAst::FunCall(_, _, t) => t
        }
    }
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}


impl std::ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Value::Integer(v) => Value::Integer(-v),
            Value::Float(v) => Value::Float(-v),
            _ => panic!("Cant take the negative of {}", self)
        }
    }
}


impl std::ops::Mul<Value> for Value {
    type Output = Self;

    fn mul(self, rhs: Value) -> Self {
        match (&self, &rhs) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l * r),
            (Value::Integer(l), Value::Float(r)) => Value::Float(*l as f32 * r),
            (Value::Float(l), Value::Integer(r)) => Value::Float(l * *r as f32),
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
            _ => panic!("Cant add {} and {}", self, rhs)
        }
    }
}

impl std::ops::Div<Value> for Value {
    type Output = Self;

    fn div(self, rhs: Value) -> Self {
        match (&self, &rhs) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l / r),
            (Value::Integer(l), Value::Float(r)) => Value::Float(*l as f32 / r),
            (Value::Float(l), Value::Integer(r)) => Value::Float(l / *r as f32),
            (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
            _ => panic!("Cant add {} and {}", self, rhs)
        }
    }
}


impl std::ops::Add<Value> for Value {
    type Output = Self;

    fn add(self, rhs: Value) -> Self {
        match (&self, &rhs) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l + r),
            (Value::Integer(l), Value::Float(r)) => Value::Float(*l as f32 + r),
            (Value::Float(l), Value::Integer(r)) => Value::Float(l + *r as f32),
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
            (Value::List(l), Value::List(r)) => {

                let mut vc = l.to_owned();
                
                for el in r {
                    vc.push(el.to_owned());
                }

                return Value::List(vc);
            },

            _ => {
                
                panic!("Cant add {} and {}", self, rhs)
            } // Should not add these
        }
    }

}


#[derive(PartialEq)]
pub struct FunDecl {
    pub fun_name: String,
    pub arguments: Vec<Type>,
    pub ret_type: Type,
    pub doc_string: String
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
    pub fn lookup_variable(&self, expr: &ExprAst) -> Option<Value> {
        match expr {
            ExprAst::Var(var_name, var_type) => match self.variables.iter().filter(|vd| &vd.var_name == var_name && &vd.var_type == var_type).collect::<Vec<&VarDecl>>().pop() {
                Some(vd) => Some(vd.value.clone()),
                None => None
            }
            _ => None
        }
    }


    pub fn lookup_function(&self, expr: &ExprAst) -> Option<FunDecl> {
        match expr {
            ExprAst::FunCall(fun_name, args, ret_type) => match self.functions.iter().filter(|fd| &fd.fun_name == fun_name && &fd.ret_type == ret_type).collect::<Vec<&FunDecl>>() {
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