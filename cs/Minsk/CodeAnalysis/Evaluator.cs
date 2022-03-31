using Minsk.CodeAnalysis.Binding;

namespace Minsk.CodeAnalysis;

internal sealed class Evaluator
{
    private readonly BoundStatement _root;
    private readonly Dictionary<VariableSymbol, object> _variables;
    private object? _lastValue;

    public Evaluator(BoundStatement root, Dictionary<VariableSymbol, object> variables)
    {
        _root = root;
        _variables = variables;
    }

    public object Evaluate()
    {
        EvaluateStatement(_root);
        return _lastValue ?? throw new InvalidOperationException();
    }

    private void EvaluateStatement(BoundStatement root)
    {
        // ReSharper disable once SwitchStatementHandlesSomeKnownEnumValuesWithDefault
        switch (root.Kind)
        {
            case BoundNodeKind.BlockStatement:
                EvaluateBlockStatement((BoundBlockStatement)root);
                break;
            case BoundNodeKind.ExpressionStatement:
                EvaluateExpressionStatement((BoundExpressionStatement)root);
                break;
            case BoundNodeKind.IfStatement:
                EvaluateIfStatement((BoundIfStatement)root);
                break;
            case BoundNodeKind.VariableDeclaration:
                EvaluateVariableDeclaration((BoundVariableDeclaration)root);
                break;
            default:
                throw new InvalidOperationException();
        }
    }

    private void EvaluateIfStatement(BoundIfStatement root)
    {
        var condition = (bool)EvaluateExpression(root.Condition);
        if (condition)
        {
            EvaluateStatement(root.ThenStatement);
        }
        else if (root.ElseStatement != null)
        {
            EvaluateStatement(root.ElseStatement);
        }
    }

    private void EvaluateVariableDeclaration(BoundVariableDeclaration root)
    {
        var initializer = EvaluateExpression(root.Initializer);
        _variables[root.Variable] = initializer;
        _lastValue = initializer;
    }

    private void EvaluateExpressionStatement(BoundExpressionStatement root)
    {
        _lastValue = EvaluateExpression(root.Expression);
    }

    private void EvaluateBlockStatement(BoundBlockStatement root)
    {
        foreach (var statement in root.Statements)
        {
            EvaluateStatement(statement);
        }
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
            BoundBinaryOperatorKind.LessThan => (int)left < (int)right,
            BoundBinaryOperatorKind.LessThanOrEqual => (int)left <= (int)right,
            BoundBinaryOperatorKind.GreaterThan => (int)left > (int)right,
            BoundBinaryOperatorKind.GreaterThanOrEqual => (int)left >= (int)right,
            _ => throw new InvalidOperationException()
        };
    }

    private static object EvaluateLiteralExpression(BoundLiteralExpression root)
    {
        return root.Value ?? throw new InvalidOperationException();
    }
}
