use crate::cdsl::{
    algebra::{Algebra, UnitType},
    expr::Expr,
    interface::{FnDecl, Sort, Value, Variable},
};

#[test]
fn test_assignment() {
    let mut alg: Algebra<i32> = Algebra::new();

    let var = Variable::new("X".to_string(), Sort::Int);
    let val = Value::new(10);

    let expr = Expr::Var(var.clone());

    alg.assign(var, val.clone());

    let res = alg.eval(expr);

    assert!(res.is_some());

    assert_eq!(res.unwrap(), val);
}

#[test]
fn test_assignment_fns() {
    let mut alg: Algebra<i32> = Algebra::new();

    let var_a = Variable::new("A".to_string(), Sort::Int);
    let var_b = Variable::new("B".to_string(), Sort::Int);
    let val = Value::new(10);

    alg.assign(var_a.clone(), val.clone());

    alg.assign(var_b.clone(), val.clone());

    let fn_decl = FnDecl::new("Add".to_string(), vec![Sort::Int, Sort::Int]);
    // Box<dyn Fn(Vec<Value<T>>) -> Value<T>>
    let fn_expr: Box<dyn Fn(Vec<Value<i32>>) -> Value<i32>> =
        Box::new(|args: Vec<Value<i32>>| match &args[..] {
            [a, b] => Value::new(a.val() + b.val()),
            _ => panic!("Invalid number of arguments"),
        });

    alg.add_fn(fn_decl.clone(), fn_expr);

    let expr = Expr::Fun(fn_decl, vec![var_a, var_b]);

    let res = alg.eval(expr);

    assert!(res.is_some());

    assert_eq!(res.unwrap().val(), 10 + 10);
}

impl UnitType for i32 {}
