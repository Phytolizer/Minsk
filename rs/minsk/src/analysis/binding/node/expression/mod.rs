use crate::object::ObjectKind;

use self::binary::BoundBinaryExpression;
use self::literal::BoundLiteralExpression;
use self::unary::BoundUnaryExpression;

use super::kind::BoundNodeKind;
use super::BoundNode;

pub(crate) mod binary;
pub(crate) mod literal;
pub(crate) mod unary;

#[derive(Debug)]
pub(crate) enum BoundExpression {
    Binary(BoundBinaryExpression),
    Literal(BoundLiteralExpression),
    Unary(BoundUnaryExpression),
}

impl BoundNode for BoundExpression {
    fn kind(&self) -> BoundNodeKind {
        match self {
            Self::Binary(_) => BoundNodeKind::BinaryExpression,
            Self::Literal(_) => BoundNodeKind::LiteralExpression,
            Self::Unary(_) => BoundNodeKind::UnaryExpression,
        }
    }
}

impl BoundExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        match self {
            Self::Binary(b) => b.ty(),
            Self::Literal(lit) => lit.ty(),
            Self::Unary(u) => u.ty(),
        }
    }
}
