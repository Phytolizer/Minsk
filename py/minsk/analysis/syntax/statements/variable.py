from dataclasses import dataclass
from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.statement import StatementSyntax
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class VariableDeclarationSyntax(StatementSyntax):
    keyword_token: SyntaxToken
    identifier_token: SyntaxToken
    equals_token: SyntaxToken
    initializer: ExpressionSyntax

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.VariableDeclaration

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.keyword_token
        yield self.identifier_token
        yield self.equals_token
        yield self.initializer
