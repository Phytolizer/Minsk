from typing import Optional

from minsk.analysis.binding.operators.binary.kind import BoundBinaryOperatorKind
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.type import MinskType


class BoundBinaryOperator:
    syntax_kind: SyntaxKind
    kind: BoundBinaryOperatorKind
    left_type: MinskType
    right_type: MinskType
    result_type: MinskType

    def __init__(
        self,
        syntax_kind: SyntaxKind,
        operator_kind: BoundBinaryOperatorKind,
        left_type: MinskType,
        right_type: MinskType,
        result_type: MinskType,
    ):
        self.syntax_kind = syntax_kind
        self.kind = operator_kind
        self.left_type = left_type
        self.right_type = right_type
        self.result_type = result_type


_OPERATORS = (
    BoundBinaryOperator(
        SyntaxKind.PlusToken,
        BoundBinaryOperatorKind.Addition,
        MinskType.Int,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundBinaryOperator(
        SyntaxKind.MinusToken,
        BoundBinaryOperatorKind.Subtraction,
        MinskType.Int,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundBinaryOperator(
        SyntaxKind.StarToken,
        BoundBinaryOperatorKind.Multiplication,
        MinskType.Int,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundBinaryOperator(
        SyntaxKind.SlashToken,
        BoundBinaryOperatorKind.Division,
        MinskType.Int,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundBinaryOperator(
        SyntaxKind.AmpersandAmpersandToken,
        BoundBinaryOperatorKind.LogicalAnd,
        MinskType.Bool,
        MinskType.Bool,
        MinskType.Bool,
    ),
    BoundBinaryOperator(
        SyntaxKind.PipePipeToken,
        BoundBinaryOperatorKind.LogicalOr,
        MinskType.Bool,
        MinskType.Bool,
        MinskType.Bool,
    ),
    BoundBinaryOperator(
        SyntaxKind.BangEqualsToken,
        BoundBinaryOperatorKind.Inequality,
        MinskType.Int,
        MinskType.Int,
        MinskType.Bool,
    ),
    BoundBinaryOperator(
        SyntaxKind.EqualsEqualsToken,
        BoundBinaryOperatorKind.Equality,
        MinskType.Int,
        MinskType.Int,
        MinskType.Bool,
    ),
    BoundBinaryOperator(
        SyntaxKind.BangEqualsToken,
        BoundBinaryOperatorKind.Inequality,
        MinskType.Bool,
        MinskType.Bool,
        MinskType.Bool,
    ),
    BoundBinaryOperator(
        SyntaxKind.EqualsEqualsToken,
        BoundBinaryOperatorKind.Equality,
        MinskType.Bool,
        MinskType.Bool,
        MinskType.Bool,
    ),
)


def bind_binary_operator(
    syntax_kind: SyntaxKind, left_type: MinskType, right_type: MinskType
) -> Optional[BoundBinaryOperator]:
    for op in _OPERATORS:
        if (
            op.syntax_kind == syntax_kind
            and op.left_type == left_type
            and op.right_type == right_type
        ):
            return op

    return None
