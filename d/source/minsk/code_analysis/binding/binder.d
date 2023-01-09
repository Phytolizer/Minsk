module minsk.code_analysis.binding.binder;

import std.format : format;

import minsk.code_analysis.binding.node : BoundExpression,
    BoundBinaryExpression,
    BoundBinaryOperatorKind,
    BoundLiteralExpression,
    BoundUnaryExpression,
    BoundUnaryOperatorKind;
import minsk.code_analysis.syntax : ExpressionSyntax,
    BinaryExpressionSyntax,
    LiteralExpressionSyntax,
    ParenthesizedExpressionSyntax,
    UnaryExpressionSyntax,
    SyntaxKind;
import minsk.runtime.object : Obj, Integer, Type;
import optional : Optional, no, some;

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

    private Optional!BoundBinaryOperatorKind bindBinaryOperatorKind(
        SyntaxKind kind,
        Type leftType,
        Type rightType
    ) {
        if (leftType == Type.Integer && rightType == Type.Integer) {
            switch (kind) {
                case SyntaxKind.PlusToken:
                    return some(BoundBinaryOperatorKind.Addition);
                case SyntaxKind.MinusToken:
                    return some(BoundBinaryOperatorKind.Subtraction);
                case SyntaxKind.StarToken:
                    return some(BoundBinaryOperatorKind.Multiplication);
                case SyntaxKind.SlashToken:
                    return some(BoundBinaryOperatorKind.Division);
                default:
                    break;
            }
        } else if (leftType == Type.Boolean && rightType == Type.Boolean) {
            switch (kind) {
                case SyntaxKind.AmpersandAmpersandToken:
                    return some(BoundBinaryOperatorKind.LogicalAnd);
                case SyntaxKind.PipePipeToken:
                    return some(BoundBinaryOperatorKind.LogicalOr);
                default:
                    break;
            }
        }
        return no!BoundBinaryOperatorKind;
    }

    private const(BoundExpression) bindBinaryExpression(const(BinaryExpressionSyntax) syntax) {
        const left = bindExpression(syntax.left);
        const right = bindExpression(syntax.right);
        const operatorKind = bindBinaryOperatorKind(
            syntax.operatorToken.kind,
            left.type,
            right.type
        );
        if (operatorKind.empty) {
            _diagnostics ~= format!"Binary operator '%s' is not defined for types '%s' and '%s'."(
                syntax.operatorToken.text,
                left.type,
                right.type,
            );
            return left;
        }

        return new BoundBinaryExpression(left, operatorKind.front, right);
    }

    private Optional!BoundUnaryOperatorKind bindUnaryOperatorKind(SyntaxKind kind, Type operandType) {
        if (operandType == Type.Integer) {
            switch (kind) {
                case SyntaxKind.PlusToken:
                    return some(BoundUnaryOperatorKind.Identity);
                case SyntaxKind.MinusToken:
                    return some(BoundUnaryOperatorKind.ArithmeticNegation);
                default:
                    break;
            }
        } else if (operandType == Type.Boolean) {
            switch (kind) {
                case SyntaxKind.BangToken:
                    return some(BoundUnaryOperatorKind.LogicalNegation);
                default:
                    break;
            }
        }
        return no!BoundUnaryOperatorKind;
    }

    private const(BoundExpression) bindUnaryExpression(const(UnaryExpressionSyntax) syntax) {
        const operand = bindExpression(syntax.operand);
        const operatorKind = bindUnaryOperatorKind(syntax.operatorToken.kind, operand.type);
        if (operatorKind.empty) {
            _diagnostics ~= format!"Unary operator '%s' is not defined for type '%s'."(
                syntax.operatorToken.text,
                operand.type,
            );
            return operand;
        }

        return new BoundUnaryExpression(operatorKind.front, operand);
    }

    private const(BoundExpression) bindLiteralExpression(const(LiteralExpressionSyntax) syntax) {
        return new BoundLiteralExpression(syntax.value);
    }

    private const(BoundExpression) bindParenthesizedExpression(const(ParenthesizedExpressionSyntax) syntax) {
        return bindExpression(syntax.expression);
    }
}
