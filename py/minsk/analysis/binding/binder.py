from typing import Any, Optional, cast

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.expressions.assignment import BoundAssignmentExpression
from minsk.analysis.binding.expressions.binary import BoundBinaryExpression
from minsk.analysis.binding.expressions.literal import BoundLiteralExpression
from minsk.analysis.binding.expressions.unary import BoundUnaryExpression
from minsk.analysis.binding.expressions.variable import BoundVariableExpression
from minsk.analysis.binding.operators.binary import bind_binary_operator
from minsk.analysis.binding.operators.unary import bind_unary_operator
from minsk.analysis.binding.scope import BoundScope
from minsk.analysis.binding.scope.globl import BoundGlobalScope
from minsk.analysis.diagnostic import Diagnostic
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
from minsk.analysis.syntax.unit import CompilationUnitSyntax
from minsk.analysis.type import MinskType
from minsk.analysis.variable import VariableSymbol


class Binder:
    _diagnostics: DiagnosticBag
    _scope: BoundScope

    def __init__(self, parent: Optional[BoundScope]):
        self._diagnostics = DiagnosticBag()
        self._scope = BoundScope(parent)

    @staticmethod
    def bind_global_scope(
        previous: Optional[BoundGlobalScope], syntax: CompilationUnitSyntax
    ) -> BoundGlobalScope:
        parent_scope = Binder._create_parent_scopes(previous)
        binder = Binder(parent_scope)
        expression = binder.bind_expression(syntax.expression)
        diagnostics = binder.diagnostics
        variables = binder._scope.declared_variables
        return BoundGlobalScope(previous, diagnostics, variables, expression)

    @staticmethod
    def _create_parent_scopes(
        previous: Optional[BoundGlobalScope],
    ) -> Optional[BoundScope]:
        stack: list[BoundGlobalScope] = []

        while previous is not None:
            stack.append(previous)
            previous = previous.previous

        current: Optional[BoundScope] = None
        while len(stack) > 0:
            previous = stack.pop()
            scope = BoundScope(current)
            for v in previous.variables:
                scope.try_declare(v)
            current = scope

        return current

    @property
    def diagnostics(self) -> tuple[Diagnostic, ...]:
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
        variable = VariableSymbol(name, expression.ty)
        if not self._scope.try_declare(variable):
            self._diagnostics.report_variable_already_declared(
                syntax.identifier_token.span, name
            )

        return BoundAssignmentExpression(variable, expression)

    def _bind_name_expression(self, syntax: NameExpressionSyntax) -> BoundExpression:
        name = syntax.identifier_token.text
        variable = self._scope.try_lookup(name)
        if variable is None:
            self._diagnostics.report_undefined_name(syntax.identifier_token.span, name)
            return BoundLiteralExpression(0)

        return BoundVariableExpression(variable)
