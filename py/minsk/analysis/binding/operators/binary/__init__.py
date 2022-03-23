from typing import Optional, Type

from minsk.analysis.binding.operators.binary.kind import BoundBinaryOperatorKind
from minsk.analysis.syntax.kind import SyntaxKind


class BoundBinaryOperator:
    syntax_kind: SyntaxKind
    kind: BoundBinaryOperatorKind
    left_type: Type
    right_type: Type
    result_type: Type

    def __init__(
        self,
        syntax_kind: SyntaxKind,
        operator_kind: BoundBinaryOperatorKind,
        left_type: Type,
        right_type: Type,
        result_type: Type,
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
        int,
        int,
        int,
    ),
    BoundBinaryOperator(
        SyntaxKind.MinusToken,
        BoundBinaryOperatorKind.Subtraction,
        int,
        int,
        int,
    ),
    BoundBinaryOperator(
        SyntaxKind.StarToken,
        BoundBinaryOperatorKind.Multiplication,
        int,
        int,
        int,
    ),
    BoundBinaryOperator(
        SyntaxKind.SlashToken,
        BoundBinaryOperatorKind.Division,
        int,
        int,
        int,
    ),
)


def bind_binary_operator(
    syntax_kind: SyntaxKind, left_type: Type, right_type: Type
) -> Optional[BoundBinaryOperator]:
    for op in _OPERATORS:
        if (
            op.syntax_kind == syntax_kind
            and op.left_type == left_type
            and op.right_type == right_type
        ):
            return op

    return None
