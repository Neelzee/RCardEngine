use crate::errors::CDSLError;

use super::ASTNode;

pub fn validate(node: ASTNode) -> Result<(), CDSLError> {
    match node.token {
        super::Token::AssignmentExpression => todo!(), // Assignment
        super::Token::Binop => todo!(),                // Binop
        super::Token::IfExpression => todo!(),         // If expression
        super::Token::FnCall => todo!(),               // Function Call
        super::Token::FnDecl => todo!(),               // Function Declaration
        super::Token::VarDecl => todo!(),              // Variable Declaration
        _ => todo!(),
    }

    Ok(())
}
