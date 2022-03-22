package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.syntax.*

class Evaluator {
    fun evaluate(root: ExpressionSyntax): Any {
        return evaluateExpression(root)
    }

    private fun evaluateExpression(root: ExpressionSyntax): Any {
        return when (root.kind) {
            SyntaxKind.BinaryExpression -> evaluateBinaryExpression(root as BinaryExpressionSyntax)
            SyntaxKind.LiteralExpression -> evaluateLiteralExpression(root as LiteralExpressionSyntax)
            SyntaxKind.ParenthesizedExpression -> evaluateParenthesizedExpression(root as ParenthesizedExpressionSyntax)
            SyntaxKind.UnaryExpression -> evaluateUnaryExpression(root as UnaryExpressionSyntax)
            else -> throw IllegalStateException()
        }
    }

    private fun evaluateUnaryExpression(root: UnaryExpressionSyntax): Any {
        val operand = evaluateExpression(root.operand)

        return when (root.operatorToken.kind) {
            SyntaxKind.PlusToken -> operand
            SyntaxKind.MinusToken -> -(operand as Int)
            else -> throw IllegalStateException()
        }
    }

    private fun evaluateParenthesizedExpression(root: ParenthesizedExpressionSyntax): Any {
        return evaluateExpression(root.expression)
    }

    private fun evaluateLiteralExpression(root: LiteralExpressionSyntax): Any {
        return root.literalToken.value!!
    }

    private fun evaluateBinaryExpression(root: BinaryExpressionSyntax): Any {
        val left = evaluateExpression(root.left)
        val right = evaluateExpression(root.right)

        return when (root.operatorToken.kind) {
            SyntaxKind.PlusToken -> left as Int + right as Int
            SyntaxKind.MinusToken -> left as Int - right as Int
            SyntaxKind.StarToken -> left as Int * right as Int
            SyntaxKind.SlashToken -> left as Int / right as Int
            else -> throw IllegalStateException()
        }
    }
}
