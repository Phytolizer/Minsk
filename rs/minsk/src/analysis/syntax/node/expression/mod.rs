use std::fmt::Display;

use crate::analysis::syntax::kind::SyntaxKind;

use self::assignment::AssignmentExpressionSyntax;
use self::binary::BinaryExpressionSyntax;
use self::literal::LiteralExpressionSyntax;
use self::name::NameExpressionSyntax;
use self::parenthesized::ParenthesizedExpressionSyntax;
use self::unary::UnaryExpressionSyntax;

use super::SyntaxNode;

pub mod assignment;
pub mod binary;
pub mod literal;
pub mod name;
pub mod parenthesized;
pub mod unary;

#[derive(Debug)]
pub enum ExpressionSyntax {
    Assignment(AssignmentExpressionSyntax),
    Binary(BinaryExpressionSyntax),
    Literal(LiteralExpressionSyntax),
    Name(NameExpressionSyntax),
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
            Self::Assignment(_) => SyntaxKind::AssignmentExpression,
            Self::Binary(_) => SyntaxKind::BinaryExpression,
            Self::Literal(_) => SyntaxKind::LiteralExpression,
            Self::Name(_) => SyntaxKind::NameExpression,
            Self::Unary(_) => SyntaxKind::UnaryExpression,
            Self::Parenthesized(_) => SyntaxKind::ParenthesizedExpression,
        }
    }

    fn children(&self) -> Vec<&dyn SyntaxNode> {
        match self {
            Self::Assignment(a) => {
                vec![&a.identifier_token, &a.equals_token, a.expression.as_ref()]
            }
            Self::Binary(b) => vec![b.left.as_ref(), &b.operator_token, b.right.as_ref()],
            Self::Literal(lit) => vec![&lit.literal_token],
            Self::Name(n) => vec![&n.identifier_token],
            Self::Unary(u) => vec![&u.operator_token, u.operand.as_ref()],
            Self::Parenthesized(p) => vec![
                &p.open_parenthesis_token,
                p.expression.as_ref(),
                &p.close_parenthesis_token,
            ],
        }
    }
}
