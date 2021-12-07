namespace Minsk.CodeAnalysis.Syntax;

public class Evaluator
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

    private static int EvaluateExpression(ExpressionSyntax root)
    {
        return root.Kind switch
        {
            SyntaxKind.BinaryExpression => EvaluateBinaryExpression((BinaryExpressionSyntax)root),
            SyntaxKind.LiteralExpression => EvaluateLiteralExpression((LiteralExpressionSyntax)root),
            SyntaxKind.ParenthesizedExpression => EvaluateParenthesizedExpression((ParenthesizedExpressionSyntax)root),
            _ => throw new InvalidOperationException()
        };
    }

    private static int EvaluateParenthesizedExpression(ParenthesizedExpressionSyntax root)
    {
        return EvaluateExpression(root.Expression);
    }

    private static int EvaluateLiteralExpression(LiteralExpressionSyntax root)
    {
        return (int)(root.LiteralToken.Value ?? throw new InvalidOperationException());
    }

    private static int EvaluateBinaryExpression(BinaryExpressionSyntax root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        return root.OperatorToken.Kind switch
        {
            SyntaxKind.PlusToken => left + right,
            SyntaxKind.MinusToken => left - right,
            SyntaxKind.StarToken => left * right,
            SyntaxKind.SlashToken => left / right,
            _ => 0
        };
    }
}