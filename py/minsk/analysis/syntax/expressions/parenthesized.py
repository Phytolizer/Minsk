from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


class ParenthesizedExpressionSyntax(ExpressionSyntax):
    def __init__(
        self,
        open_parenthesis_token: SyntaxToken,
        expression: ExpressionSyntax,
        close_parenthesis_token: SyntaxToken,
    ):
        self.open_parenthesis_token = open_parenthesis_token
        self.expression = expression
        self.close_parenthesis_token = close_parenthesis_token

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.ParenthesizedExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.open_parenthesis_token
        yield self.expression
        yield self.close_parenthesis_token
