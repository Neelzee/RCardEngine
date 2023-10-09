use crate::cdsl::error::Error;
use crate::cdsl::expr::*;

fn eval(expr: ExprAst, scope: &Scope) -> Result<Value, Error> {
    match expr {
        ExprAst::Var(_, _) => match scope.lookup_variable(&expr) {
            None => Err(Error {}),
            Some(v) => Ok(v)
        }

        ExprAst::Add(l, r) => {
            if l.get_type() != r.get_type() {
                return Err(Error {});
            }

            match eval(*l, scope) {
                Ok(l_v) => match eval(*r, scope) {
                    Ok(r_v) => Ok(l_v + r_v),
                    Err(e) => Err(e)
                }
                Err(e) => Err(e)
            }
        }

        ExprAst::Neg(expr) => match eval(*expr, scope) {
            Ok(v) => Ok(-v),
            Err(e) => Err(e)
        }

        ExprAst::Sub(l, r) => eval(ExprAst::Neg(Box::new(ExprAst::Add(l, r))), scope),

        ExprAst::Mult(l, r) => {
            if l.get_type() != r.get_type() {
                return Err(Error {});
            }

            match eval(*l, scope) {
                Ok(l_v) => match eval(*r, scope) {
                    Ok(r_v) => Ok(l_v * r_v),
                    Err(e) => Err(e)
                }
                Err(e) => Err(e)
            }
        }

        ExprAst::Div(l, r) => {
            if l.get_type() != r.get_type() {
                return Err(Error {});
            }

            match eval(*l, scope) {
                Ok(l_v) => match eval(*r, scope) {
                    Ok(r_v) => Ok(l_v / r_v),
                    Err(e) => Err(e)
                }
                Err(e) => Err(e)
            }
        }

        ExprAst::FunCall(_, _, _) => match scope.lookup_function(&expr) {
            None => Err(Error {}),
            Some(fn_decl) => todo!()
        }
    }
}