package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.DiagnosticBag
import dev.phytolizer.minsk.analysis.syntax.*

internal class Binder(private val _variables: MutableMap<String, Any>) {
    private val _diagnostics = DiagnosticBag()
    val diagnostics: List<Diagnostic>
        get() = _diagnostics.diagnostics

    fun bindExpression(syntax: ExpressionSyntax): BoundExpression = when (syntax.kind) {
        SyntaxKind.AssignmentExpression -> bindAssignmentExpression(syntax as AssignmentExpressionSyntax)
        SyntaxKind.BinaryExpression -> bindBinaryExpression(syntax as BinaryExpressionSyntax)
        SyntaxKind.LiteralExpression -> bindLiteralExpression(syntax as LiteralExpressionSyntax)
        SyntaxKind.NameExpression -> bindNameExpression(syntax as NameExpressionSyntax)
        SyntaxKind.ParenthesizedExpression -> bindParenthesizedExpression(syntax as ParenthesizedExpressionSyntax)
        SyntaxKind.UnaryExpression -> bindUnaryExpression(syntax as UnaryExpressionSyntax)
        else -> throw IllegalStateException()
    }

    private fun bindAssignmentExpression(syntax: AssignmentExpressionSyntax): BoundExpression {
        val expression = bindExpression(syntax.expression)

        val name = syntax.identifierToken.text
        val existingVariable = _variables[name]
        if (existingVariable != null) {
            _variables.remove(existingVariable)
        }

        _variables[name] = when (expression.type) {
            Int::class -> 0
            Boolean::class -> false
            else -> throw IllegalStateException()
        }
        return BoundAssignmentExpression(name, expression)
    }

    private fun bindBinaryExpression(syntax: BinaryExpressionSyntax): BoundExpression {
        val left = bindExpression(syntax.left)
        val right = bindExpression(syntax.right)

        val op = BoundBinaryOperator.bind(syntax.operatorToken.kind, left.type, right.type)
        return if (op == null) {
            _diagnostics.reportUndefinedBinaryOperator(
                syntax.operatorToken.span,
                syntax.operatorToken.text,
                left.type,
                right.type,
            )
            left
        } else {
            BoundBinaryExpression(left, op, right)
        }
    }

    private fun bindLiteralExpression(syntax: LiteralExpressionSyntax): BoundExpression {
        return BoundLiteralExpression(syntax.value ?: 0)
    }

    private fun bindNameExpression(syntax: NameExpressionSyntax): BoundExpression {
        val name = syntax.identifierToken.text
        val value = _variables[name]
        return if (value == null) {
            _diagnostics.reportUndefinedName(syntax.identifierToken.span, name)
            BoundLiteralExpression(0)
        } else {
            val type = value::class
            BoundVariableExpression(name, type)
        }
    }

    private fun bindParenthesizedExpression(syntax: ParenthesizedExpressionSyntax): BoundExpression {
        return bindExpression(syntax.expression)
    }

    private fun bindUnaryExpression(syntax: UnaryExpressionSyntax): BoundExpression {
        val operand = bindExpression(syntax.operand)

        val op = BoundUnaryOperator.bind(syntax.operatorToken.kind, operand.type)
        return if (op == null) {
            _diagnostics.reportUndefinedUnaryOperator(
                syntax.operatorToken.span,
                syntax.operatorToken.text,
                operand.type,
            )
            operand
        } else {
            BoundUnaryExpression(op, operand)
        }
    }
}
