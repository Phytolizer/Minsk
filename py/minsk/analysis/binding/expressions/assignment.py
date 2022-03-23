from dataclasses import dataclass

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.type import MinskType
from minsk.analysis.variable import VariableSymbol


@dataclass(frozen=True)
class BoundAssignmentExpression(BoundExpression):
    variable: VariableSymbol
    expression: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.AssignmentExpression

    @property
    def ty(self) -> MinskType:
        return self.variable.ty
