use crate::analysis::syntax::kind::SyntaxKind;
use crate::object::ObjectKind;

use self::kind::BoundUnaryOperatorKind;

pub(crate) mod kind;

#[derive(Debug)]
pub(crate) struct BoundUnaryOperator {
    syntax_kind: SyntaxKind,
    pub(crate) kind: BoundUnaryOperatorKind,
    operand_type: ObjectKind,
    pub(crate) result_type: ObjectKind,
}

impl BoundUnaryOperator {
    const fn new(
        syntax_kind: SyntaxKind,
        kind: BoundUnaryOperatorKind,
        operand_type: ObjectKind,
        result_type: ObjectKind,
    ) -> Self {
        Self {
            syntax_kind,
            kind,
            operand_type,
            result_type,
        }
    }

    pub(crate) fn bind(
        syntax_kind: SyntaxKind,
        operand_type: ObjectKind,
    ) -> Option<&'static BoundUnaryOperator> {
        UNARY_OPERATORS
            .iter()
            .find(|op| op.syntax_kind == syntax_kind && op.operand_type == operand_type)
    }
}

const UNARY_OPERATORS: &[BoundUnaryOperator] = &[
    BoundUnaryOperator::new(
        SyntaxKind::PlusToken,
        BoundUnaryOperatorKind::Identity,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundUnaryOperator::new(
        SyntaxKind::MinusToken,
        BoundUnaryOperatorKind::Negation,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundUnaryOperator::new(
        SyntaxKind::BangToken,
        BoundUnaryOperatorKind::LogicalNegation,
        ObjectKind::Bool,
        ObjectKind::Bool,
    ),
];
