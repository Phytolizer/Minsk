use std::fmt::Display;

use crate::analysis::syntax::kind::SyntaxKind;

use self::binary::BinaryExpressionSyntax;
use self::literal::LiteralExpressionSyntax;
use self::parenthesized::ParenthesizedExpressionSyntax;
use self::unary::UnaryExpressionSyntax;

use super::SyntaxNode;

pub mod binary;
pub mod literal;
pub mod parenthesized;
pub mod unary;

#[derive(Debug)]
pub enum ExpressionSyntax {
    Binary(BinaryExpressionSyntax),
    Literal(LiteralExpressionSyntax),
    Parenthesized(ParenthesizedExpressionSyntax),
    Unary(UnaryExpressionSyntax),
}

impl Display for ExpressionSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl SyntaxNode for ExpressionSyntax {
    fn kind(&self) -> SyntaxKind {
        match self {
            Self::Binary(_) => SyntaxKind::BinaryExpression,
            Self::Literal(_) => SyntaxKind::LiteralExpression,
            Self::Unary(_) => SyntaxKind::UnaryExpression,
            Self::Parenthesized(_) => SyntaxKind::ParenthesizedExpression,
        }
    }

    fn children(&self) -> Vec<&dyn SyntaxNode> {
        match self {
            Self::Binary(b) => vec![b.left.as_ref(), &b.operator_token, b.right.as_ref()],
            Self::Literal(lit) => vec![&lit.literal_token],
            Self::Unary(u) => vec![&u.operator_token, u.operand.as_ref()],
            Self::Parenthesized(p) => vec![
                &p.open_parenthesis_token,
                p.expression.as_ref(),
                &p.close_parenthesis_token,
            ],
        }
    }
}
