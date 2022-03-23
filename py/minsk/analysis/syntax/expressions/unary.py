from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


class UnaryExpressionSyntax(ExpressionSyntax):
    operator_token: SyntaxToken
    operand: ExpressionSyntax

    def __init__(self, operator_token: SyntaxToken, operand: ExpressionSyntax):
        self.operator_token = operator_token
        self.operand = operand

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.UnaryExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.operator_token
        yield self.operand
