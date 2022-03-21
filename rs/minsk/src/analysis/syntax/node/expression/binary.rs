use crate::analysis::syntax::token::SyntaxToken;

use super::ExpressionSyntax;

#[derive(Debug)]
pub struct BinaryExpressionSyntax {
    pub left: Box<ExpressionSyntax>,
    pub operator_token: SyntaxToken,
    pub right: Box<ExpressionSyntax>,
}
