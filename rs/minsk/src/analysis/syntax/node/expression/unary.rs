use crate::analysis::syntax::token::SyntaxToken;

use super::ExpressionSyntax;

#[derive(Debug)]
pub struct UnaryExpressionSyntax {
    pub operator_token: SyntaxToken,
    pub operand: Box<ExpressionSyntax>,
}
