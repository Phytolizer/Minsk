package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.*

internal class Evaluator(private val _variables: MutableMap<String, Any>) {
    fun evaluate(root: BoundExpression): Any {
        return evaluateExpression(root)
    }

    private fun evaluateExpression(root: BoundExpression): Any {
        return when (root.kind) {
            BoundNodeKind.AssignmentExpression -> evaluateAssignmentExpression(root as BoundAssignmentExpression)
            BoundNodeKind.BinaryExpression -> evaluateBinaryExpression(root as BoundBinaryExpression)
            BoundNodeKind.LiteralExpression -> evaluateLiteralExpression(root as BoundLiteralExpression)
            BoundNodeKind.UnaryExpression -> evaluateUnaryExpression(root as BoundUnaryExpression)
            BoundNodeKind.VariableExpression -> evaluateVariableExpression(root as BoundVariableExpression)
        }
    }

    private fun evaluateAssignmentExpression(root: BoundAssignmentExpression): Any {
        val value = evaluateExpression(root.expression)
        _variables[root.name] = value
        return value
    }

    private fun evaluateBinaryExpression(root: BoundBinaryExpression): Any {
        val left = evaluateExpression(root.left)
        val right = evaluateExpression(root.right)

        return when (root.op.kind) {
            BoundBinaryOperatorKind.Addition -> left as Int + right as Int
            BoundBinaryOperatorKind.Subtraction -> left as Int - right as Int
            BoundBinaryOperatorKind.Multiplication -> left as Int * right as Int
            BoundBinaryOperatorKind.Division -> left as Int / right as Int
            BoundBinaryOperatorKind.LogicalAnd -> left as Boolean && right as Boolean
            BoundBinaryOperatorKind.LogicalOr -> left as Boolean || right as Boolean
            BoundBinaryOperatorKind.Equality -> left == right
            BoundBinaryOperatorKind.Inequality -> left != right
        }
    }

    private fun evaluateLiteralExpression(root: BoundLiteralExpression): Any {
        return root.value
    }

    private fun evaluateUnaryExpression(root: BoundUnaryExpression): Any {
        val operand = evaluateExpression(root.operand)

        return when (root.op.kind) {
            BoundUnaryOperatorKind.Identity -> operand
            BoundUnaryOperatorKind.Negation -> -(operand as Int)
            BoundUnaryOperatorKind.LogicalNegation -> !(operand as Boolean)
        }
    }

    private fun evaluateVariableExpression(root: BoundVariableExpression): Any {
        return _variables[root.name]!!
    }
}
