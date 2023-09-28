pub enum Expr {
    Var(String, Type),
    Func(Vec<Type>, Type)
}


pub enum Type {
    Int,
    String,
    Float,
    List(Box<Type>),
    Card,
    Player
}


pub struct Value<T> {
    val: T
}


pub struct FuncDecl {
    args: Vec<Type>,
    ret_type: Type
}


pub struct VarDecl(Vec<(String, Type)>);


pub struct Scope {
    fun_decl: Vec<FuncDecl>,
    var_decl: Vec<VarDecl>
}