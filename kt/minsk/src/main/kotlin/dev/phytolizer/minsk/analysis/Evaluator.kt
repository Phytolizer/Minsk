package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.*

internal class Evaluator(private val _variables: MutableMap<VariableSymbol, Any>) {
    private var _lastValue: Any? = null

    fun evaluate(root: BoundStatement): Any {
        evaluateStatement(root)
        return _lastValue!!
    }

    private fun evaluateStatement(root: BoundStatement) = when (root.kind) {
        BoundNodeKind.BlockStatement -> evaluateBlockStatement(root as BoundBlockStatement)
        BoundNodeKind.ExpressionStatement -> evaluateExpressionStatement(root as BoundExpressionStatement)
        BoundNodeKind.IfStatement -> evaluateIfStatement(root as BoundIfStatement)
        BoundNodeKind.VariableDeclaration -> evaluateVariableDeclaration(root as BoundVariableDeclaration)
        else -> throw IllegalStateException()
    }

    private fun evaluateIfStatement(root: BoundIfStatement) {
        val condition = evaluateExpression(root.condition) as Boolean
        if (condition) {
            evaluateStatement(root.thenStatement)
        } else if (root.elseStatement != null) {
            evaluateStatement(root.elseStatement)
        }
    }

    private fun evaluateBlockStatement(root: BoundBlockStatement) {
        for (statement in root.statements) {
            evaluateStatement(statement)
        }
    }

    private fun evaluateExpressionStatement(root: BoundExpressionStatement) {
        _lastValue = evaluateExpression(root.expression)
    }

    private fun evaluateVariableDeclaration(root: BoundVariableDeclaration) {
        val value = evaluateExpression(root.initializer)
        _variables[root.variable] = value
        _lastValue = value
    }

    private fun evaluateExpression(root: BoundExpression): Any = when (root.kind) {
        BoundNodeKind.AssignmentExpression -> evaluateAssignmentExpression(root as BoundAssignmentExpression)
        BoundNodeKind.BinaryExpression -> evaluateBinaryExpression(root as BoundBinaryExpression)
        BoundNodeKind.LiteralExpression -> evaluateLiteralExpression(root as BoundLiteralExpression)
        BoundNodeKind.UnaryExpression -> evaluateUnaryExpression(root as BoundUnaryExpression)
        BoundNodeKind.VariableExpression -> evaluateVariableExpression(root as BoundVariableExpression)
        else -> throw IllegalStateException()
    }

    private fun evaluateAssignmentExpression(root: BoundAssignmentExpression): Any {
        val value = evaluateExpression(root.expression)
        _variables[root.variable] = value
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
            BoundBinaryOperatorKind.LessThan -> (left as Int) < (right as Int)
            BoundBinaryOperatorKind.LessOrEquals -> (left as Int) <= (right as Int)
            BoundBinaryOperatorKind.GreaterThan -> (left as Int) > (right as Int)
            BoundBinaryOperatorKind.GreaterOrEquals -> (left as Int) >= (right as Int)
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
        return _variables[root.variable]!!
    }
}
