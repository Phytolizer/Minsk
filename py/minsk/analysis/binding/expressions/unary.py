from dataclasses import dataclass
from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.unary import BoundUnaryOperator


@dataclass(frozen=True)
class BoundUnaryExpression(BoundExpression):
    operator: BoundUnaryOperator
    operand: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.UnaryExpression

    @property
    def ty(self) -> Type:
        return self.operator.result_type
