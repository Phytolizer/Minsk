use crate::analysis::syntax::token::SyntaxToken;
use crate::object::Object;

#[derive(Debug)]
pub struct LiteralExpressionSyntax {
    pub literal_token: SyntaxToken,
    pub value: Option<Object>,
}
