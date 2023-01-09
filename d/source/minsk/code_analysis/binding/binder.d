module minsk.code_analysis.binding.binder;

import std.format : format;

import minsk.code_analysis.binding.node : BoundExpression,
    BoundBinaryExpression,
    BoundBinaryOperatorKind,
    BoundLiteralExpression,
    BoundUnaryExpression,
    BoundUnaryOperatorKind,
    bindBinaryOperator,
    bindUnaryOperator;
import minsk.code_analysis.syntax : ExpressionSyntax,
    BinaryExpressionSyntax,
    LiteralExpressionSyntax,
    ParenthesizedExpressionSyntax,
    UnaryExpressionSyntax,
    SyntaxKind;
import minsk.runtime.object : Obj, Integer, Type;

class Binder {
    private string[] _diagnostics = [];

    const(string[]) diagnostics() const {
        return _diagnostics;
    }

    const(BoundExpression) bindExpression(const(ExpressionSyntax) syntax) {
        switch (syntax.kind) {
            case SyntaxKind.BinaryExpression:
                return bindBinaryExpression(cast(BinaryExpressionSyntax) syntax);
            case SyntaxKind.LiteralExpression:
                return bindLiteralExpression(cast(LiteralExpressionSyntax) syntax);
            case SyntaxKind.ParenthesizedExpression:
                return bindParenthesizedExpression(cast(ParenthesizedExpressionSyntax) syntax);
            case SyntaxKind.UnaryExpression:
                return bindUnaryExpression(cast(UnaryExpressionSyntax) syntax);
            default:
                assert(false);
        }
    }

    private const(BoundExpression) bindBinaryExpression(const(BinaryExpressionSyntax) syntax) {
        const left = bindExpression(syntax.left);
        const right = bindExpression(syntax.right);
        const operator = bindBinaryOperator(
            syntax.operatorToken.kind,
            left.type,
            right.type,
        );
        if (operator.empty) {
            _diagnostics ~= format!"Binary operator '%s' is not defined for types '%s' and '%s'."(
                syntax.operatorToken.text,
                left.type,
                right.type,
            );
            return left;
        }

        return new BoundBinaryExpression(left, operator.front, right);
    }

    private const(BoundExpression) bindUnaryExpression(const(UnaryExpressionSyntax) syntax) {
        const operand = bindExpression(syntax.operand);
        const operator = bindUnaryOperator(syntax.operatorToken.kind, operand.type);
        if (operator.empty) {
            _diagnostics ~= format!"Unary operator '%s' is not defined for type '%s'."(
                syntax.operatorToken.text,
                operand.type,
            );
            return operand;
        }

        return new BoundUnaryExpression(operator.front, operand);
    }

    private const(BoundExpression) bindLiteralExpression(const(LiteralExpressionSyntax) syntax) {
        return new BoundLiteralExpression(syntax.value);
    }

    private const(BoundExpression) bindParenthesizedExpression(const(ParenthesizedExpressionSyntax) syntax) {
        return bindExpression(syntax.expression);
    }
}
