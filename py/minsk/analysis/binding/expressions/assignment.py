from dataclasses import dataclass
from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind


@dataclass(frozen=True)
class BoundAssignmentExpression(BoundExpression):
    name: str
    expression: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.AssignmentExpression

    @property
    def ty(self) -> Type:
        return self.expression.ty
