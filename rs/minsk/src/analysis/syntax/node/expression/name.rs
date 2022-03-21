use crate::analysis::syntax::token::SyntaxToken;

#[derive(Debug)]
pub struct NameExpressionSyntax {
    pub identifier_token: SyntaxToken,
}
