from typing import Any, cast

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.expressions.binary import BoundBinaryExpression
from minsk.analysis.binding.expressions.literal import BoundLiteralExpression
from minsk.analysis.binding.expressions.unary import BoundUnaryExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.binary import BoundBinaryOperatorKind
from minsk.analysis.binding.operators.unary import BoundUnaryOperatorKind


class Evaluator:
    syntax: BoundExpression

    def __init__(self, syntax: BoundExpression):
        self.syntax = syntax

    def evaluate(self) -> Any:
        return self._evaluate_expression(self.syntax)

    def _evaluate_expression(self, syntax: BoundExpression) -> Any:
        match syntax.kind:
            case BoundNodeKind.BinaryExpression:
                return self._evaluate_binary_expression(
                    cast(BoundBinaryExpression, syntax)
                )
            case BoundNodeKind.LiteralExpression:
                return self._evaluate_literal_expression(
                    cast(BoundLiteralExpression, syntax)
                )
            case BoundNodeKind.UnaryExpression:
                return self._evaluate_unary_expression(
                    cast(BoundUnaryExpression, syntax)
                )
            case _:
                raise Exception(f"unexpected syntax {syntax.kind}")

    def _evaluate_binary_expression(self, syntax: BoundBinaryExpression) -> Any:
        left = self._evaluate_expression(syntax.left)
        right = self._evaluate_expression(syntax.right)

        match syntax.operator.kind:
            case BoundBinaryOperatorKind.Addition:
                return left + right
            case BoundBinaryOperatorKind.Subtraction:
                return left - right
            case BoundBinaryOperatorKind.Multiplication:
                return left * right
            case BoundBinaryOperatorKind.Division:
                return left // right
            case BoundBinaryOperatorKind.LogicalAnd:
                return left and right
            case BoundBinaryOperatorKind.LogicalOr:
                return left or right
            case BoundBinaryOperatorKind.Equality:
                return left == right
            case BoundBinaryOperatorKind.Inequality:
                return left != right
            case _:
                raise Exception(f"Unhandled operator kind {syntax.operator.kind}")

    @staticmethod
    def _evaluate_literal_expression(syntax: BoundLiteralExpression) -> Any:
        return syntax.value

    def _evaluate_unary_expression(self, syntax: BoundUnaryExpression) -> Any:
        operand = self._evaluate_expression(syntax.operand)

        match syntax.operator.kind:
            case BoundUnaryOperatorKind.Identity:
                return operand
            case BoundUnaryOperatorKind.Negation:
                return -operand
            case BoundUnaryOperatorKind.LogicalNegation:
                return not operand
            case _:
                raise Exception(f"Unhandled operator kind {syntax.operator.kind}")
