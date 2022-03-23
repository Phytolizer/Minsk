from dataclasses import dataclass
from typing import Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind


@dataclass(frozen=True)
class BoundVariableExpression(BoundExpression):
    name: str
    ty: Type

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.VariableExpression
