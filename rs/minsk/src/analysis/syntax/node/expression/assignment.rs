use crate::analysis::syntax::token::SyntaxToken;

use super::ExpressionSyntax;

#[derive(Debug)]
pub struct AssignmentExpressionSyntax {
    pub identifier_token: SyntaxToken,
    pub equals_token: SyntaxToken,
    pub expression: Box<ExpressionSyntax>,
}
