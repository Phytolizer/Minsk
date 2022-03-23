from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.unary import BoundUnaryOperator


class BoundUnaryExpression(BoundExpression):
    operator: BoundUnaryOperator
    operand: BoundExpression

    def __init__(self, operator: BoundUnaryOperator, operand: BoundExpression):
        self.operator = operator
        self.operand = operand

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.UnaryExpression

    @property
    def ty(self) -> Type:
        return self.operator.result_type
