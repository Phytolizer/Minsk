from typing import Any, cast

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
from minsk.analysis.syntax.expressions.parenthesized import \
    ParenthesizedExpressionSyntax
from minsk.analysis.syntax.expressions.unary import UnaryExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind


class Evaluator:
    syntax: ExpressionSyntax

    def __init__(self, syntax: ExpressionSyntax):
        self.syntax = syntax

    def evaluate(self) -> Any:
        return self._evaluate_expression(self.syntax)

    def _evaluate_expression(self, syntax: ExpressionSyntax) -> Any:
        match syntax.kind:
            case SyntaxKind.BinaryExpression:
                return self._evaluate_binary_expression(
                    cast(BinaryExpressionSyntax, syntax)
                )
            case SyntaxKind.LiteralExpression:
                return self._evaluate_literal_expression(
                    cast(LiteralExpressionSyntax, syntax)
                )
            case SyntaxKind.ParenthesizedExpression:
                return self._evaluate_parenthesized_expression(
                    cast(ParenthesizedExpressionSyntax, syntax)
                )
            case SyntaxKind.UnaryExpression:
                return self._evaluate_unary_expression(
                    cast(UnaryExpressionSyntax, syntax)
                )
            case _:
                raise Exception(f"unexpected syntax {syntax.kind}")

    def _evaluate_binary_expression(self, syntax: BinaryExpressionSyntax) -> Any:
        left = self._evaluate_expression(syntax.left)
        right = self._evaluate_expression(syntax.right)

        match syntax.operator_token.kind:
            case SyntaxKind.PlusToken:
                return left + right
            case SyntaxKind.MinusToken:
                return left - right
            case SyntaxKind.StarToken:
                return left * right
            case SyntaxKind.SlashToken:
                return left // right
            case _:
                raise Exception(
                    f"unexpected binary operator {syntax.operator_token.kind}"
                )

    @staticmethod
    def _evaluate_literal_expression(syntax: LiteralExpressionSyntax) -> Any:
        return syntax.literal_token.value

    def _evaluate_parenthesized_expression(
        self, syntax: ParenthesizedExpressionSyntax
    ) -> Any:
        return self._evaluate_expression(syntax.expression)

    def _evaluate_unary_expression(self, syntax: UnaryExpressionSyntax) -> Any:
        operand = self._evaluate_expression(syntax.operand)

        match syntax.operator_token.kind:
            case SyntaxKind.PlusToken:
                return operand
            case SyntaxKind.MinusToken:
                return -operand
            case _:
                raise Exception(
                    f"unexpected unary operator {syntax.operator_token.kind}"
                )
