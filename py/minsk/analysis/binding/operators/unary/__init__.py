from typing import Optional, Type

from minsk.analysis.binding.operators.unary.kind import BoundUnaryOperatorKind
from minsk.analysis.syntax.kind import SyntaxKind


class BoundUnaryOperator:
    syntax_kind: SyntaxKind
    kind: BoundUnaryOperatorKind
    operand_type: Type
    result_type: Type

    def __init__(
        self,
        syntax_kind: SyntaxKind,
        operator_kind: BoundUnaryOperatorKind,
        operand_type: Type,
        result_type: Type,
    ):
        self.syntax_kind = syntax_kind
        self.kind = operator_kind
        self.operand_type = operand_type
        self.result_type = result_type


_OPERATORS = (
    BoundUnaryOperator(
        SyntaxKind.PlusToken,
        BoundUnaryOperatorKind.Identity,
        int,
        int,
    ),
    BoundUnaryOperator(
        SyntaxKind.MinusToken,
        BoundUnaryOperatorKind.Negation,
        int,
        int,
    ),
)


def bind_unary_operator(
    syntax_kind: SyntaxKind, operand_type: Type
) -> Optional[BoundUnaryOperator]:
    for op in _OPERATORS:
        if op.syntax_kind == syntax_kind and op.operand_type == operand_type:
            return op

    return None
