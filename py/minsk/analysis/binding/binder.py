from typing import cast

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.expressions.binary import BoundBinaryExpression
from minsk.analysis.binding.expressions.literal import BoundLiteralExpression
from minsk.analysis.binding.expressions.unary import BoundUnaryExpression
from minsk.analysis.binding.operators.binary import bind_binary_operator
from minsk.analysis.binding.operators.unary import bind_unary_operator
from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
from minsk.analysis.syntax.expressions.parenthesized import (
    ParenthesizedExpressionSyntax,
)
from minsk.analysis.syntax.expressions.unary import UnaryExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind


class Binder:
    _diagnostics: list[str]

    def __init__(self):
        self._diagnostics = []

    @property
    def diagnostics(self) -> tuple[str, ...]:
        return tuple(iter(self._diagnostics))

    def bind_expression(self, syntax: ExpressionSyntax) -> BoundExpression:
        match syntax.kind:
            case SyntaxKind.BinaryExpression:
                return self._bind_binary_expression(
                    cast(BinaryExpressionSyntax, syntax)
                )
            case SyntaxKind.LiteralExpression:
                return self._bind_literal_expression(
                    cast(LiteralExpressionSyntax, syntax)
                )
            case SyntaxKind.ParenthesizedExpression:
                return self._bind_parenthesized_expression(
                    cast(ParenthesizedExpressionSyntax, syntax)
                )
            case SyntaxKind.UnaryExpression:
                return self._bind_unary_expression(cast(UnaryExpressionSyntax, syntax))
            case _:
                raise Exception(f"Unexpected syntax {syntax.kind}")

    def _bind_binary_expression(
        self, syntax: BinaryExpressionSyntax
    ) -> BoundExpression:
        left = self.bind_expression(syntax.left)
        right = self.bind_expression(syntax.right)
        op = bind_binary_operator(syntax.operator_token.kind, left.ty, right.ty)
        if op is None:
            self._diagnostics.append(
                f"Binary operator '{syntax.operator_token.text}' isn't defined "
                + f"for types {left.ty} and {right.ty}"
            )
            return left

        return BoundBinaryExpression(left, op, right)

    @staticmethod
    def _bind_literal_expression(syntax: LiteralExpressionSyntax) -> BoundExpression:
        return BoundLiteralExpression(syntax.value)

    def _bind_parenthesized_expression(
        self, syntax: ParenthesizedExpressionSyntax
    ) -> BoundExpression:
        return self.bind_expression(syntax.expression)

    def _bind_unary_expression(self, syntax: UnaryExpressionSyntax) -> BoundExpression:
        operand = self.bind_expression(syntax.operand)
        op = bind_unary_operator(syntax.operator_token.kind, operand.ty)
        if op is None:
            self._diagnostics.append(
                f"Unary operator '{syntax.operator_token.text}' "
                + f"isn't defined for type {operand.ty}"
            )
            return operand

        return BoundUnaryExpression(op, operand)
