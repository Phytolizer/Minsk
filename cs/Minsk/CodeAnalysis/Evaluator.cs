using Minsk.CodeAnalysis.Binding;

namespace Minsk.CodeAnalysis;

internal sealed class Evaluator
{
    private readonly BoundExpression _root;
    private readonly Dictionary<VariableSymbol, object> _variables;

    public Evaluator(BoundExpression root, Dictionary<VariableSymbol, object> variables)
    {
        _root = root;
        _variables = variables;
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
            BoundNodeKind.VariableExpression => EvaluateVariableExpression((BoundVariableExpression)root),
            BoundNodeKind.AssignmentExpression => EvaluateAssignmentExpression((BoundAssignmentExpression)root),
            _ => throw new InvalidOperationException($"Unexpected syntax node {root.Kind}")
        };
    }

    private object EvaluateAssignmentExpression(BoundAssignmentExpression root)
    {
        var value = EvaluateExpression(root.Expression);
        _variables[root.Variable] = value;
        return value;
    }

    private object EvaluateVariableExpression(BoundVariableExpression root)
    {
        return _variables[root.Variable];
    }

    private object EvaluateUnaryExpression(BoundUnaryExpression root)
    {
        var operand = EvaluateExpression(root.Operand);
        return root.Op.OperatorKind switch
        {
            BoundUnaryOperatorKind.Identity => operand,
            BoundUnaryOperatorKind.Negation => -(int)operand,
            BoundUnaryOperatorKind.LogicalNegation => !(bool)operand,
            _ => throw new InvalidOperationException()
        };
    }

    private object EvaluateBinaryExpression(BoundBinaryExpression root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        return root.Op.OperatorKind switch
        {
            BoundBinaryOperatorKind.Addition => (int)left + (int)right,
            BoundBinaryOperatorKind.Subtraction => (int)left - (int)right,
            BoundBinaryOperatorKind.Multiplication => (int)left * (int)right,
            BoundBinaryOperatorKind.Division => (int)left / (int)right,
            BoundBinaryOperatorKind.LogicalAnd => (bool)left && (bool)right,
            BoundBinaryOperatorKind.LogicalOr => (bool)left || (bool)right,
            BoundBinaryOperatorKind.Equality => left.Equals(right),
            BoundBinaryOperatorKind.Inequality => !left.Equals(right),
            _ => throw new InvalidOperationException()
        };
    }

    private static object EvaluateLiteralExpression(BoundLiteralExpression root)
    {
        return root.Value ?? throw new InvalidOperationException();
    }
}
