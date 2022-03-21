use crate::analysis::syntax::token::SyntaxToken;

use super::ExpressionSyntax;

#[derive(Debug)]
pub struct ParenthesizedExpressionSyntax {
    pub open_parenthesis_token: SyntaxToken,
    pub expression: Box<ExpressionSyntax>,
    pub close_parenthesis_token: SyntaxToken,
}
