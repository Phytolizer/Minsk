from dataclasses import dataclass
from typing import Iterable

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.statement import StatementSyntax
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class BlockStatementSyntax(StatementSyntax):
    open_brace_token: SyntaxToken
    statements: tuple[StatementSyntax, ...]
    close_brace_token: SyntaxToken

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.BlockStatement

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.open_brace_token
        for statement in self.statements:
            yield statement
        yield self.close_brace_token
