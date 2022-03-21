use crate::analysis::syntax::kind::SyntaxKind;
use crate::object::ObjectKind;

use self::kind::BoundBinaryOperatorKind;

pub(crate) mod kind;

#[derive(Debug)]
pub(crate) struct BoundBinaryOperator {
    syntax_kind: SyntaxKind,
    pub(crate) kind: BoundBinaryOperatorKind,
    left_type: ObjectKind,
    right_type: ObjectKind,
    pub(crate) result_type: ObjectKind,
}

impl BoundBinaryOperator {
    const fn new(
        syntax_kind: SyntaxKind,
        kind: BoundBinaryOperatorKind,
        left_type: ObjectKind,
        right_type: ObjectKind,
        result_type: ObjectKind,
    ) -> Self {
        Self {
            syntax_kind,
            kind,
            left_type,
            right_type,
            result_type,
        }
    }

    pub(crate) fn bind(
        operator_kind: SyntaxKind,
        left_type: ObjectKind,
        right_type: ObjectKind,
    ) -> Option<&'static BoundBinaryOperator> {
        BINARY_OPERATORS.iter().find(|op| {
            op.syntax_kind == operator_kind
                && op.left_type == left_type
                && op.right_type == right_type
        })
    }
}

const BINARY_OPERATORS: &[BoundBinaryOperator] = &[
    BoundBinaryOperator::new(
        SyntaxKind::PlusToken,
        BoundBinaryOperatorKind::Addition,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::MinusToken,
        BoundBinaryOperatorKind::Subtraction,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::StarToken,
        BoundBinaryOperatorKind::Multiplication,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::SlashToken,
        BoundBinaryOperatorKind::Division,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::AmpersandAmpersandToken,
        BoundBinaryOperatorKind::LogicalAnd,
        ObjectKind::Bool,
        ObjectKind::Bool,
        ObjectKind::Bool,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::PipePipeToken,
        BoundBinaryOperatorKind::LogicalOr,
        ObjectKind::Bool,
        ObjectKind::Bool,
        ObjectKind::Bool,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::EqualsEqualsToken,
        BoundBinaryOperatorKind::Equality,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Bool,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::BangEqualsToken,
        BoundBinaryOperatorKind::Inequality,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Bool,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::EqualsEqualsToken,
        BoundBinaryOperatorKind::Equality,
        ObjectKind::Bool,
        ObjectKind::Bool,
        ObjectKind::Bool,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::BangEqualsToken,
        BoundBinaryOperatorKind::Inequality,
        ObjectKind::Bool,
        ObjectKind::Bool,
        ObjectKind::Bool,
    ),
];
