using Minsk.CodeAnalysis.Binding;

namespace Minsk.CodeAnalysis;

internal sealed class Evaluator
{
    private readonly BoundExpression _root;

    public Evaluator(BoundExpression root)
    {
        _root = root;
    }

    public int Evaluate()
    {
        return EvaluateExpression(_root);
    }

    private int EvaluateExpression(BoundExpression root)
    {
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Kind switch
        {
            BoundNodeKind.LiteralExpression => EvaluateLiteralExpression((BoundLiteralExpression)root),
            BoundNodeKind.BinaryExpression => EvaluateBinaryExpression((BoundBinaryExpression)root),
            BoundNodeKind.UnaryExpression => EvaluateUnaryExpression((BoundUnaryExpression)root),
            _ => throw new InvalidOperationException($"Unexpected syntax node {root.Kind}")
        };
    }

    private int EvaluateUnaryExpression(BoundUnaryExpression root)
    {
        var operand = EvaluateExpression(root.Operand);
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Op.OperatorKind switch
        {
            BoundUnaryOperatorKind.Identity => operand,
            BoundUnaryOperatorKind.Negation => -operand,
            _ => throw new InvalidOperationException(),
        };
    }

    private int EvaluateBinaryExpression(BoundBinaryExpression root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Op.OperatorKind switch
        {
            BoundBinaryOperatorKind.Addition => left + right,
            BoundBinaryOperatorKind.Subtraction => left - right,
            BoundBinaryOperatorKind.Multiplication => left * right,
            BoundBinaryOperatorKind.Division => left / right,
            _ => throw new InvalidOperationException(),
        };
    }

    private static int EvaluateLiteralExpression(BoundLiteralExpression root)
    {
        return root.Value as int? ?? throw new InvalidOperationException();
    }
}