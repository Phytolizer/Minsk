from dataclasses import dataclass

from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.statement import BoundStatement


@dataclass(frozen=True)
class BoundBlockStatement(BoundStatement):
    statements: tuple[BoundStatement, ...]

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.BlockStatement
