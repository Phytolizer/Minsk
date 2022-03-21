using Minsk.CodeAnalysis.Binding;

namespace Minsk.CodeAnalysis;

internal sealed class Evaluator
{
    private readonly BoundExpression _root;

    public Evaluator(BoundExpression root)
    {
        _root = root;
    }

    public object Evaluate()
    {
        return EvaluateExpression(_root);
    }

    private object EvaluateExpression(BoundExpression root)
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

    private object EvaluateUnaryExpression(BoundUnaryExpression root)
    {
        var operand = EvaluateExpression(root.Operand);
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Op.OperatorKind switch
        {
            BoundUnaryOperatorKind.Identity => operand,
            BoundUnaryOperatorKind.Negation => -(int)operand,
            _ => throw new InvalidOperationException(),
        };
    }

    private object EvaluateBinaryExpression(BoundBinaryExpression root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return root.Op.OperatorKind switch
        {
            BoundBinaryOperatorKind.Addition => (int)left + (int)right,
            BoundBinaryOperatorKind.Subtraction => (int)left - (int)right,
            BoundBinaryOperatorKind.Multiplication => (int)left * (int)right,
            BoundBinaryOperatorKind.Division => (int)left / (int)right,
            _ => throw new InvalidOperationException(),
        };
    }

    private static object EvaluateLiteralExpression(BoundLiteralExpression root)
    {
        return root.Value ?? throw new InvalidOperationException();
    }
}