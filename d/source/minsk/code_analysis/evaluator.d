module minsk.code_analysis.evaluator;

import minsk.code_analysis.syntax : BinaryExpressionSyntax,
    ExpressionSyntax,
    LiteralExpressionSyntax,
    SyntaxKind;
import minsk.runtime.object : Integer, Obj;

final class Evaluator {
    private const(ExpressionSyntax) _root;

    this(const(ExpressionSyntax) root) {
        _root = root;
    }

    int evaluate() {
        return evaluateExpression(_root);
    }

    private int evaluateExpression(const(ExpressionSyntax) root) {
        switch (root.kind) {
            case SyntaxKind.LiteralExpression:
                return evaluateLiteralExpression(cast(LiteralExpressionSyntax) root);
            case SyntaxKind.BinaryExpression:
                return evaluateBinaryExpression(cast(BinaryExpressionSyntax) root);
            default:
                assert(false);
        }
    }

    private int evaluateLiteralExpression(LiteralExpressionSyntax root) {
        return (cast(Integer) root.literalToken.value).value;
    }

    private int evaluateBinaryExpression(BinaryExpressionSyntax root) {
        const left = evaluateExpression(root.left);
        const right = evaluateExpression(root.right);
        switch (root.operatorToken.kind) {
            case SyntaxKind.PlusToken:
                return left + right;
            case SyntaxKind.MinusToken:
                return left - right;
            case SyntaxKind.StarToken:
                return left * right;
            case SyntaxKind.SlashToken:
                return left / right;
            default:
                assert(false);
        }
    }
}
