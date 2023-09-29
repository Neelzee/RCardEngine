use crate::cdsl::expr::*;

fn eval(expr: Expr, scope: Scope) -> Value {
    match expr {
        Expr::Var(var_name, var_type) => todo!(),
        Expr::FunCall(fun_name, args, fun_type) => todo!()
    }
}