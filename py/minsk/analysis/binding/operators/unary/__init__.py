
from minsk.analysis.binding.operators.unary.kind import BoundUnaryOperatorKind
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.type import MinskType


class BoundUnaryOperator:
    syntax_kind: SyntaxKind
    kind: BoundUnaryOperatorKind
    operand_type: MinskType
    result_type: MinskType

    def __init__(
        self,
        syntax_kind: SyntaxKind,
        operator_kind: BoundUnaryOperatorKind,
        operand_type: MinskType,
        result_type: MinskType,
    ) -> None:
        self.syntax_kind = syntax_kind
        self.kind = operator_kind
        self.operand_type = operand_type
        self.result_type = result_type


_OPERATORS = (
    BoundUnaryOperator(
        SyntaxKind.PlusToken,
        BoundUnaryOperatorKind.Identity,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundUnaryOperator(
        SyntaxKind.MinusToken,
        BoundUnaryOperatorKind.Negation,
        MinskType.Int,
        MinskType.Int,
    ),
    BoundUnaryOperator(
        SyntaxKind.BangToken,
        BoundUnaryOperatorKind.LogicalNegation,
        MinskType.Bool,
        MinskType.Bool,
    ),
)


def bind_unary_operator(
    syntax_kind: SyntaxKind, operand_type: MinskType
) -> BoundUnaryOperator | None:
    for op in _OPERATORS:
        if op.syntax_kind == syntax_kind and op.operand_type == operand_type:
            return op

    return None
