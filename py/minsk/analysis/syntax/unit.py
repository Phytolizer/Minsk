from dataclasses import dataclass
from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class CompilationUnitSyntax(SyntaxNode):
    expression: ExpressionSyntax
    end_of_file_token: SyntaxToken

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.CompilationUnit

    @property
    def children(self) -> Iterable["SyntaxNode"]:
        yield self.expression
        yield self.end_of_file_token
