from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.binary import BoundBinaryOperator


class BoundBinaryExpression(BoundExpression):
    left: BoundExpression
    operator: BoundBinaryOperator
    right: BoundExpression

    def __init__(
        self,
        left: BoundExpression,
        operator: BoundBinaryOperator,
        right: BoundExpression,
    ):
        self.left = left
        self.operator = operator
        self.right = right

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.BinaryExpression

    @property
    def ty(self) -> Type:
        return self.operator.result_type
