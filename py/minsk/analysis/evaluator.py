from typing import Any, Optional, cast

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.expressions.assignment import BoundAssignmentExpression
from minsk.analysis.binding.expressions.binary import BoundBinaryExpression
from minsk.analysis.binding.expressions.literal import BoundLiteralExpression
from minsk.analysis.binding.expressions.unary import BoundUnaryExpression
from minsk.analysis.binding.expressions.variable import BoundVariableExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.binding.operators.binary import BoundBinaryOperatorKind
from minsk.analysis.binding.operators.unary import BoundUnaryOperatorKind
from minsk.analysis.binding.statement import BoundStatement
from minsk.analysis.binding.statements.block import BoundBlockStatement
from minsk.analysis.binding.statements.expression import BoundExpressionStatement
from minsk.analysis.binding.statements.variable import BoundVariableDeclaration
from minsk.analysis.variable import VariableSymbol


class Evaluator:
    _statement: BoundStatement
    _variables: dict[VariableSymbol, Any]
    _last_value: Optional[Any]

    def __init__(self, statement: BoundStatement, variables: dict[VariableSymbol, Any]):
        self._statement = statement
        self._variables = variables
        self._last_value = None

    def evaluate(self) -> Any:
        self._evaluate_statement(self._statement)
        return self._last_value

    def _evaluate_statement(self, statement: BoundStatement):
        match statement.kind:
            case BoundNodeKind.BlockStatement:
                self._evaluate_block_statement(cast(BoundBlockStatement, statement))
            case BoundNodeKind.VariableDeclaration:
                self._evaluate_variable_declaration(
                    cast(BoundVariableDeclaration, statement)
                )
            case BoundNodeKind.ExpressionStatement:
                self._evaluate_expression_statement(
                    cast(BoundExpressionStatement, statement)
                )

    def _evaluate_block_statement(self, statement: BoundBlockStatement):
        for s in statement.statements:
            self._evaluate_statement(s)

    def _evaluate_variable_declaration(self, statement: BoundVariableDeclaration):
        value = self._evaluate_expression(statement.initializer)
        self._variables[statement.variable] = value
        self._last_value = value

    def _evaluate_expression_statement(self, statement: BoundExpressionStatement):
        self._last_value = self._evaluate_expression(statement.expression)

    def _evaluate_expression(self, syntax: BoundExpression) -> Any:
        match syntax.kind:
            case BoundNodeKind.AssignmentExpression:
                return self._evaluate_assignment_expression(
                    cast(BoundAssignmentExpression, syntax)
                )
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
            case BoundNodeKind.VariableExpression:
                return self._evaluate_variable_expression(
                    cast(BoundVariableExpression, syntax)
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

    def _evaluate_assignment_expression(self, syntax: BoundAssignmentExpression) -> Any:
        value = self._evaluate_expression(syntax.expression)
        self._variables[syntax.variable] = value
        return value

    def _evaluate_variable_expression(self, syntax: BoundVariableExpression) -> Any:
        return self._variables[syntax.variable]
