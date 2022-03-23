from dataclasses import dataclass

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.statement import BoundStatement


@dataclass(frozen=True)
class BoundExpressionStatement(BoundStatement):
    expression: BoundExpression

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.ExpressionStatement
