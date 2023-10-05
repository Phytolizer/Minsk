from collections.abc import Iterable
from dataclasses import dataclass

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.statement import StatementSyntax
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class CompilationUnitSyntax(SyntaxNode):
    statement: StatementSyntax
    end_of_file_token: SyntaxToken

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.CompilationUnit

    @property
    def children(self) -> Iterable["SyntaxNode"]:
        yield self.statement
        yield self.end_of_file_token
