module minsk.code_analysis.evaluator;

import minsk.code_analysis.binding : BoundNodeKind,
    BoundBinaryExpression,
    BoundBinaryOperatorKind,
    BoundExpression,
    BoundLiteralExpression,
    BoundUnaryExpression,
    BoundUnaryOperatorKind;
import minsk.runtime.object : Integer, Obj;

final class Evaluator {
    private const(BoundExpression) _root;

    this(const(BoundExpression) root) {
        _root = root;
    }

    int evaluate() {
        return evaluateExpression(_root);
    }

    private int evaluateExpression(const(BoundExpression) root) {
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

    private int evaluateLiteralExpression(BoundLiteralExpression root) {
        return (cast(Integer) root.value).value;
    }

    private int evaluateBinaryExpression(BoundBinaryExpression root) {
        const left = evaluateExpression(root.left);
        const right = evaluateExpression(root.right);
        final switch (root.operatorKind) {
            case BoundBinaryOperatorKind.Addition:
                return left + right;
            case BoundBinaryOperatorKind.Subtraction:
                return left - right;
            case BoundBinaryOperatorKind.Multiplication:
                return left * right;
            case BoundBinaryOperatorKind.Division:
                return left / right;
        }
    }

    private int evaluateUnaryExpression(BoundUnaryExpression root) {
        const operand = evaluateExpression(root.operand);
        final switch (root.operatorKind) {
            case BoundUnaryOperatorKind.Identity:
                return operand;
            case BoundUnaryOperatorKind.Negation:
                return -operand;
        }
    }
}
