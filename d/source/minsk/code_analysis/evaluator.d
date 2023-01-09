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
        const left = evaluateExpression(root.left).intVal;
        const right = evaluateExpression(root.right).intVal;
        final switch (root.operatorKind) {
            case BoundBinaryOperatorKind.Addition:
                return new Integer(left + right);
            case BoundBinaryOperatorKind.Subtraction:
                return new Integer(left - right);
            case BoundBinaryOperatorKind.Multiplication:
                return new Integer(left * right);
            case BoundBinaryOperatorKind.Division:
                return new Integer(left / right);
        }
    }

    private const(Obj) evaluateUnaryExpression(BoundUnaryExpression root) {
        const operand = evaluateExpression(root.operand).intVal;
        final switch (root.operatorKind) {
            case BoundUnaryOperatorKind.Identity:
                return new Integer(operand);
            case BoundUnaryOperatorKind.Negation:
                return new Integer(-operand);
        }
    }
}
