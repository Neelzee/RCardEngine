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

#[derive(Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    List(Vec<Value>),
    Card(Card),
    Player(Player)
}


pub struct FunDecl {
    fun_name: String,
    arguments: Vec<Type>,
    ret_type: Type
}


pub struct VarDecl {
    var_name: String,
    var_type: Type,
    value: Value
}



pub struct Scope {
    variables: Vec<VarDecl>,
    functions: Vec<FunDecl>
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
            Expr::FunCall(fun_name, args, ret_type) => match self.functions.iter().filter(|fd| &fd.fun_name == fun_name && &vd.ret_type == ret_type).collect::<Vec<&VarDecl>>() {
                fs => {
                    for f in fs {
                        
                    }
                }
            }
            _ => None
        }
    }
}