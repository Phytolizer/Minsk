package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.syntax.*

internal class Binder {
    private val _diagnostics = mutableListOf<String>()
    val diagnostics: List<String>
        get() = _diagnostics

    fun bindExpression(syntax: ExpressionSyntax): BoundExpression = when (syntax.kind) {
        SyntaxKind.BinaryExpression -> bindBinaryExpression(syntax as BinaryExpressionSyntax)
        SyntaxKind.LiteralExpression -> bindLiteralExpression(syntax as LiteralExpressionSyntax)
        SyntaxKind.ParenthesizedExpression -> bindParenthesizedExpression(syntax as ParenthesizedExpressionSyntax)
        SyntaxKind.UnaryExpression -> bindUnaryExpression(syntax as UnaryExpressionSyntax)
        else -> throw IllegalStateException()
    }

    private fun bindBinaryExpression(syntax: BinaryExpressionSyntax): BoundExpression {
        val left = bindExpression(syntax.left)
        val right = bindExpression(syntax.right)

        val op = BoundBinaryOperator.bind(syntax.operatorToken.kind, left.type, right.type)
        return if (op == null) {
            left
        } else {
            BoundBinaryExpression(left, op, right)
        }
    }

    private fun bindLiteralExpression(syntax: LiteralExpressionSyntax): BoundExpression {
        return BoundLiteralExpression(syntax.literalToken.value!!)
    }

    private fun bindParenthesizedExpression(syntax: ParenthesizedExpressionSyntax): BoundExpression {
        return bindExpression(syntax.expression)
    }

    private fun bindUnaryExpression(syntax: UnaryExpressionSyntax): BoundExpression {
        val operand = bindExpression(syntax.operand)

        val op = BoundUnaryOperator.bind(syntax.operatorToken.kind, operand.type)
        return if (op == null) {
            operand
        } else {
            BoundUnaryExpression(op, operand)
        }
    }
}
