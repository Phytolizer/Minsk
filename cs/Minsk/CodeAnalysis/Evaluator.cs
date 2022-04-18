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
            case BoundNodeKind.ForStatement:
                EvaluateForStatement((BoundForStatement)root);
                break;
            case BoundNodeKind.VariableDeclaration:
                EvaluateVariableDeclaration((BoundVariableDeclaration)root);
                break;
            case BoundNodeKind.WhileStatement:
                EvaluateWhileStatement((BoundWhileStatement)root);
                break;
            default:
                throw new InvalidOperationException();
        }
    }

    private void EvaluateForStatement(BoundForStatement root)
    {
        var lowerBound = (int)EvaluateExpression(root.LowerBound);
        var upperBound = (int)EvaluateExpression(root.UpperBound);

        for (var i = lowerBound; i <= upperBound; i++)
        {
            _variables[root.Variable] = i;
            EvaluateStatement(root.Body);
        }
    }

    private void EvaluateWhileStatement(BoundWhileStatement root)
    {
        while ((bool)EvaluateExpression(root.Condition))
        {
            EvaluateStatement(root.Body);
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
            BoundUnaryOperatorKind.BitwiseNegation => ~(int)operand,
            _ => throw new InvalidOperationException()
        };
    }

    private object EvaluateBinaryExpression(BoundBinaryExpression root)
    {
        var left = EvaluateExpression(root.Left);
        var right = EvaluateExpression(root.Right);

        switch (root.Op.OperatorKind)
        {
            case BoundBinaryOperatorKind.Addition:
                return (int)left + (int)right;
            case BoundBinaryOperatorKind.Subtraction:
                return (int)left - (int)right;
            case BoundBinaryOperatorKind.Multiplication:
                return (int)left * (int)right;
            case BoundBinaryOperatorKind.Division:
                return (int)left / (int)right;
            case BoundBinaryOperatorKind.LogicalAnd:
                return (bool)left && (bool)right;
            case BoundBinaryOperatorKind.LogicalOr:
                return (bool)left || (bool)right;
            case BoundBinaryOperatorKind.Equality:
                return left.Equals(right);
            case BoundBinaryOperatorKind.Inequality:
                return !left.Equals(right);
            case BoundBinaryOperatorKind.LessThan:
                return (int)left < (int)right;
            case BoundBinaryOperatorKind.LessThanOrEqual:
                return (int)left <= (int)right;
            case BoundBinaryOperatorKind.GreaterThan:
                return (int)left > (int)right;
            case BoundBinaryOperatorKind.GreaterThanOrEqual:
                return (int)left >= (int)right;
            case BoundBinaryOperatorKind.BitwiseAnd:
                {
                    if (root.Type == typeof(int))
                    {
                        return (int)left & (int)right;
                    }
                    else if (root.Type == typeof(bool))
                    {
                        return (bool)left && (bool)right;
                    }
                    else
                    {
                        throw new InvalidOperationException();
                    }
                }
            case BoundBinaryOperatorKind.BitwiseOr:
                {
                    if (root.Type == typeof(int))
                    {
                        return (int)left | (int)right;
                    }
                    else if (root.Type == typeof(bool))
                    {
                        return (bool)left || (bool)right;
                    }
                    else
                    {
                        throw new InvalidOperationException();
                    }
                }
            case BoundBinaryOperatorKind.BitwiseXor:
                {
                    if (root.Type == typeof(int))
                    {
                        return (int)left ^ (int)right;
                    }
                    else if (root.Type == typeof(bool))
                    {
                        return (bool)left ^ (bool)right;
                    }
                    else
                    {
                        throw new InvalidOperationException();
                    }
                }
            default:
                throw new InvalidOperationException();
        };
    }

    private static object EvaluateLiteralExpression(BoundLiteralExpression root)
    {
        return root.Value ?? throw new InvalidOperationException();
    }
}
