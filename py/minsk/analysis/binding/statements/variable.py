from dataclasses import dataclass

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.statement import BoundStatement
from minsk.analysis.variable import VariableSymbol


@dataclass(frozen=True)
class BoundVariableDeclaration(BoundStatement):
    variable: VariableSymbol
    initializer: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.VariableDeclaration
