from typing import Optional, cast

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
from minsk.analysis.binding.statement import BoundStatement
from minsk.analysis.binding.statements.block import BoundBlockStatement
from minsk.analysis.binding.statements.expression import BoundExpressionStatement
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
from minsk.analysis.syntax.statement import StatementSyntax
from minsk.analysis.syntax.statements.block import BlockStatementSyntax
from minsk.analysis.syntax.statements.expression import ExpressionStatementSyntax
from minsk.analysis.syntax.unit import CompilationUnitSyntax
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
        statement = binder.bind_statement(syntax.statement)
        diagnostics = binder.diagnostics
        variables = binder._scope.declared_variables

        if previous is not None:
            diagnostics = previous.diagnostics + diagnostics

        return BoundGlobalScope(previous, diagnostics, variables, statement)

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

    def bind_statement(self, syntax: StatementSyntax) -> BoundStatement:
        match syntax.kind:
            case SyntaxKind.BlockStatement:
                return self._bind_block_statement(cast(BlockStatementSyntax, syntax))
            case SyntaxKind.ExpressionStatement:
                return self._bind_expression_statement(
                    cast(ExpressionStatementSyntax, syntax)
                )
            case _:
                raise Exception(f"Unhandled syntax {syntax.kind}")

    def _bind_block_statement(self, syntax: BlockStatementSyntax) -> BoundStatement:
        statements: list[BoundStatement] = []
        for statement in syntax.statements:
            bound_statement = self.bind_statement(statement)
            statements.append(bound_statement)

        return BoundBlockStatement(tuple(statements))

    def _bind_expression_statement(
        self, syntax: ExpressionStatementSyntax
    ) -> BoundStatement:
        expression = self._bind_expression(syntax.expression)
        return BoundExpressionStatement(expression)

    def _bind_expression(self, syntax: ExpressionSyntax) -> BoundExpression:
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
        left = self._bind_expression(syntax.left)
        right = self._bind_expression(syntax.right)
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
        return self._bind_expression(syntax.expression)

    def _bind_unary_expression(self, syntax: UnaryExpressionSyntax) -> BoundExpression:
        operand = self._bind_expression(syntax.operand)
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
        expression = self._bind_expression(syntax.expression)
        name = syntax.identifier_token.text
        variable = self._scope.try_lookup(name)
        if variable is None:
            variable = VariableSymbol(name, expression.ty)
            self._scope.try_declare(variable)

        if expression.ty != variable.ty:
            self._diagnostics.report_cannot_convert(
                syntax.equals_token.span, variable.ty, expression.ty
            )

        return BoundAssignmentExpression(variable, expression)

    def _bind_name_expression(self, syntax: NameExpressionSyntax) -> BoundExpression:
        name = syntax.identifier_token.text
        variable = self._scope.try_lookup(name)
        if variable is None:
            self._diagnostics.report_undefined_name(syntax.identifier_token.span, name)
            return BoundLiteralExpression(0)

        return BoundVariableExpression(variable)
