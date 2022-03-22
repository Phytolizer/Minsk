package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.*

internal class Evaluator {
    fun evaluate(root: BoundExpression): Any {
        return evaluateExpression(root)
    }

    private fun evaluateExpression(root: BoundExpression): Any {
        return when (root.kind) {
            BoundNodeKind.BinaryExpression -> evaluateBinaryExpression(root as BoundBinaryExpression)
            BoundNodeKind.LiteralExpression -> evaluateLiteralExpression(root as BoundLiteralExpression)
            BoundNodeKind.UnaryExpression -> evaluateUnaryExpression(root as BoundUnaryExpression)
        }
    }

    private fun evaluateUnaryExpression(root: BoundUnaryExpression): Any {
        val operand = evaluateExpression(root.operand)

        return when (root.op.kind) {
            BoundUnaryOperatorKind.Identity -> operand
            BoundUnaryOperatorKind.Negation -> -(operand as Int)
            BoundUnaryOperatorKind.LogicalNegation -> !(operand as Boolean)
        }
    }

    private fun evaluateLiteralExpression(root: BoundLiteralExpression): Any {
        return root.value
    }

    private fun evaluateBinaryExpression(root: BoundBinaryExpression): Any {
        val left = evaluateExpression(root.left)
        val right = evaluateExpression(root.right)

        return when (root.op.kind) {
            BoundBinaryOperatorKind.Addition -> left as Int + right as Int
            BoundBinaryOperatorKind.Subtraction -> left as Int - right as Int
            BoundBinaryOperatorKind.Multiplication -> left as Int * right as Int
            BoundBinaryOperatorKind.Division -> left as Int / right as Int
        }
    }
}
