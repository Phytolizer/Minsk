from dataclasses import dataclass
from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class AssignmentExpressionSyntax(ExpressionSyntax):
    identifier_token: SyntaxToken
    equals_token: SyntaxToken
    expression: ExpressionSyntax

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.AssignmentExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.identifier_token
        yield self.equals_token
        yield self.expression
