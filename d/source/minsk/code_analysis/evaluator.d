module minsk.code_analysis.evaluator;

import minsk.code_analysis.binding : BoundNodeKind,
    BoundBinaryExpression,
    BoundBinaryOperatorKind,
    BoundExpression,
    BoundLiteralExpression,
    BoundUnaryExpression,
    BoundUnaryOperatorKind;
import minsk.runtime.object : Boolean, Integer, Obj;

private int intVal(const(Obj) o) {
    return (cast(Integer) o).value;
}

private bool boolVal(const(Obj) o) {
    return (cast(Boolean) o).value;
}

final class Evaluator {
    private const(BoundExpression) _root;

    this(const(BoundExpression) root) {
        _root = root;
    }

    const(Obj) evaluate() {
        return evaluateExpression(_root);
    }

    private const(Obj) evaluateExpression(const(BoundExpression) root) {
        switch (root.kind) {
            case BoundNodeKind.LiteralExpression:
                return evaluateLiteralExpression(cast(BoundLiteralExpression) root);
            case BoundNodeKind.BinaryExpression:
                return evaluateBinaryExpression(cast(BoundBinaryExpression) root);
            case BoundNodeKind.UnaryExpression:
                return evaluateUnaryExpression(cast(BoundUnaryExpression) root);
            default:
                assert(false);
        }
    }

    private const(Obj) evaluateLiteralExpression(BoundLiteralExpression root) {
        return root.value;
    }

    private const(Obj) evaluateBinaryExpression(BoundBinaryExpression root) {
        const left = evaluateExpression(root.left);
        const right = evaluateExpression(root.right);
        final switch (root.operator.kind) {
            case BoundBinaryOperatorKind.Addition:
                return new Integer(left.intVal + right.intVal);
            case BoundBinaryOperatorKind.Subtraction:
                return new Integer(left.intVal - right.intVal);
            case BoundBinaryOperatorKind.Multiplication:
                return new Integer(left.intVal * right.intVal);
            case BoundBinaryOperatorKind.Division:
                return new Integer(left.intVal / right.intVal);
            case BoundBinaryOperatorKind.LogicalAnd:
                return new Boolean(left.boolVal && right.boolVal);
            case BoundBinaryOperatorKind.LogicalOr:
                return new Boolean(left.boolVal || right.boolVal);
            case BoundBinaryOperatorKind.Equality:
                return new Boolean(left == right);
            case BoundBinaryOperatorKind.Inequality:
                return new Boolean(left != right);
        }
    }

    private const(Obj) evaluateUnaryExpression(BoundUnaryExpression root) {
        const operand = evaluateExpression(root.operand);
        final switch (root.operator.kind) {
            case BoundUnaryOperatorKind.Identity:
                return new Integer(operand.intVal);
            case BoundUnaryOperatorKind.ArithmeticNegation:
                return new Integer(-operand.intVal);
            case BoundUnaryOperatorKind.LogicalNegation:
                return new Boolean(!operand.boolVal);
        }
    }
}
