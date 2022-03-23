from dataclasses import dataclass
from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.binary import BoundBinaryOperator


@dataclass(frozen=True)
class BoundBinaryExpression(BoundExpression):
    left: BoundExpression
    operator: BoundBinaryOperator
    right: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.BinaryExpression

    @property
    def ty(self) -> Type:
        return self.operator.result_type
