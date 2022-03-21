use crate::object::ObjectKind;

use self::assignment::BoundAssignmentExpression;
use self::binary::BoundBinaryExpression;
use self::literal::BoundLiteralExpression;
use self::unary::BoundUnaryExpression;
use self::variable::BoundVariableExpression;

use super::kind::BoundNodeKind;
use super::BoundNode;

pub(crate) mod assignment;
pub(crate) mod binary;
pub(crate) mod literal;
pub(crate) mod unary;
pub(crate) mod variable;

#[derive(Debug)]
pub(crate) enum BoundExpression {
    Assignment(BoundAssignmentExpression),
    Binary(BoundBinaryExpression),
    Literal(BoundLiteralExpression),
    Unary(BoundUnaryExpression),
    Variable(BoundVariableExpression),
}

impl BoundNode for BoundExpression {
    fn kind(&self) -> BoundNodeKind {
        match self {
            Self::Assignment(_) => BoundNodeKind::AssignmentExpression,
            Self::Binary(_) => BoundNodeKind::BinaryExpression,
            Self::Literal(_) => BoundNodeKind::LiteralExpression,
            Self::Unary(_) => BoundNodeKind::UnaryExpression,
            Self::Variable(_) => BoundNodeKind::VariableExpression,
        }
    }
}

impl BoundExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        match self {
            Self::Assignment(a) => a.ty(),
            Self::Binary(b) => b.ty(),
            Self::Literal(lit) => lit.ty(),
            Self::Unary(u) => u.ty(),
            Self::Variable(v) => v.ty(),
        }
    }
}
