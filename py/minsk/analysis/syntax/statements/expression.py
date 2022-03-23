from dataclasses import dataclass
from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.statement import StatementSyntax


@dataclass
class ExpressionStatementSyntax(StatementSyntax):
    expression: ExpressionSyntax

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.ExpressionStatement

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.expression
