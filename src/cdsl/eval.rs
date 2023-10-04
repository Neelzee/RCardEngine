use crate::cdsl::expr::*;

fn eval(expr: ExprAst, scope: Scope) -> Value {
    match expr {
        ExprAst::Var(var_name, var_type) => todo!(),
        ExprAst::FunCall(fun_name, args, fun_type) => todo!()
    }
}