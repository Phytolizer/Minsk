from typing import Any, cast

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.expressions.assignment import BoundAssignmentExpression
from minsk.analysis.binding.expressions.binary import BoundBinaryExpression
from minsk.analysis.binding.expressions.literal import BoundLiteralExpression
from minsk.analysis.binding.expressions.unary import BoundUnaryExpression
from minsk.analysis.binding.expressions.variable import BoundVariableExpression
from minsk.analysis.binding.operators.binary import bind_binary_operator
from minsk.analysis.binding.operators.unary import bind_unary_operator
from minsk.analysis.diagnostic.bag import DiagnosticBag
from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.assignment import AssignmentExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
from minsk.analysis.syntax.expressions.name import NameExpressionSyntax
from minsk.analysis.syntax.expressions.parenthesized import (
    ParenthesizedExpressionSyntax,
)
from minsk.analysis.syntax.expressions.unary import UnaryExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.type import MinskType
from minsk.analysis.variable import VariableSymbol


class Binder:
    _diagnostics: DiagnosticBag
    _variables: dict[VariableSymbol, Any]

    def __init__(self, variables: dict[VariableSymbol, Any]):
        self._diagnostics = DiagnosticBag()
        self._variables = variables

    @property
    def diagnostics(self) -> tuple[str, ...]:
        return tuple(iter(self._diagnostics))

    def bind_expression(self, syntax: ExpressionSyntax) -> BoundExpression:
        match syntax.kind:
            case SyntaxKind.AssignmentExpression:
                return self._bind_assignment_expression(
                    cast(AssignmentExpressionSyntax, syntax)
                )
            case SyntaxKind.BinaryExpression:
                return self._bind_binary_expression(
                    cast(BinaryExpressionSyntax, syntax)
                )
            case SyntaxKind.LiteralExpression:
                return self._bind_literal_expression(
                    cast(LiteralExpressionSyntax, syntax)
                )
            case SyntaxKind.NameExpression:
                return self._bind_name_expression(cast(NameExpressionSyntax, syntax))
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
            self._diagnostics.report_undefined_binary_operator(
                syntax.operator_token.span,
                syntax.operator_token.text,
                left.ty,
                right.ty,
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
            self._diagnostics.report_undefined_unary_operator(
                syntax.operator_token.span, syntax.operator_token.text, operand.ty
            )
            return operand

        return BoundUnaryExpression(op, operand)

    def _bind_assignment_expression(
        self, syntax: AssignmentExpressionSyntax
    ) -> BoundExpression:
        expression = self.bind_expression(syntax.expression)
        name = syntax.identifier_token.text
        existing_variable = next(
            (v for v in self._variables.keys() if v.name == name), None
        )
        if existing_variable is not None:
            del self._variables[existing_variable]

        if expression.ty == MinskType.Int:
            default_value = 0
        elif expression.ty == MinskType.Bool:
            default_value = False
        else:
            raise Exception(f"unsupported type {expression.ty}")

        variable = VariableSymbol(name, expression.ty)
        self._variables[variable] = default_value
        return BoundAssignmentExpression(variable, expression)

    def _bind_name_expression(self, syntax: NameExpressionSyntax) -> BoundExpression:
        name = syntax.identifier_token.text
        variable = next((v for v in self._variables.keys() if v.name == name), None)
        if variable is None:
            self._diagnostics.report_undefined_name(syntax.identifier_token.span, name)
            return BoundLiteralExpression(0)

        return BoundVariableExpression(variable)
