using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis;

public sealed class Evaluator
{
    private readonly ExpressionSyntax _root;

    public Evaluator(ExpressionSyntax root)
    {
        _root = root;
    }

    public int Evaluate()
    {
        return EvaluateExpression(_root);
    }

    private int EvaluateExpression(ExpressionSyntax root)
    {
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Kind switch
        {
            SyntaxKind.LiteralExpression => EvaluateLiteralExpression((LiteralExpressionSyntax)root),
            SyntaxKind.BinaryExpression => EvaluateBinaryExpression((BinaryExpressionSyntax)root),
            SyntaxKind.UnaryExpression => EvaluateUnaryExpression((UnaryExpressionSyntax)root),
            SyntaxKind.ParenthesizedExpression => EvaluateParenthesizedExpression((ParenthesizedExpressionSyntax)root),
            _ => throw new InvalidOperationException($"Unexpected syntax node {root.Kind}")
        };
    }

    private int EvaluateUnaryExpression(UnaryExpressionSyntax root)
    {
        var operand = EvaluateExpression(root.Operand);
        return root.OperatorToken.Kind switch
        {
            SyntaxKind.PlusToken => operand,
            SyntaxKind.MinusToken => -operand,
            _ => throw new InvalidOperationException($"Unexpected unary operator {root.OperatorToken.Kind}"),
        };
    }

    private int EvaluateParenthesizedExpression(ParenthesizedExpressionSyntax root)
    {
        return EvaluateExpression(root.Expression);
    }

    private int EvaluateBinaryExpression(BinaryExpressionSyntax root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.OperatorToken.Kind switch
        {
            SyntaxKind.PlusToken => left + right,
            SyntaxKind.MinusToken => left - right,
            SyntaxKind.StarToken => left * right,
            SyntaxKind.SlashToken => left / right,
            _ => throw new InvalidOperationException($"Unexpected binary operator {root.OperatorToken.Kind}")
        };
    }

    private static int EvaluateLiteralExpression(LiteralExpressionSyntax root)
    {
        return root.LiteralToken.Value as int? ?? throw new InvalidOperationException();
    }
}