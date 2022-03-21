using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class Binder
{
    private readonly Dictionary<string, object> _variables;
    private readonly DiagnosticBag _diagnostics = new();

    public Binder(Dictionary<string, object> variables)
    {
        _variables = variables;
    }

    public IEnumerable<Diagnostic> Diagnostics => _diagnostics;

    public BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return syntax.Kind switch
        {
            SyntaxKind.BinaryExpression => BindBinaryExpression((BinaryExpressionSyntax)syntax),
            SyntaxKind.LiteralExpression => BindLiteralExpression((LiteralExpressionSyntax)syntax),
            SyntaxKind.UnaryExpression => BindUnaryExpression((UnaryExpressionSyntax)syntax),
            SyntaxKind.ParenthesizedExpression => BindParenthesizedExpression((ParenthesizedExpressionSyntax)syntax),
            SyntaxKind.NameExpression => BindNameExpression((NameExpressionSyntax)syntax),
            SyntaxKind.AssignmentExpression => BindAssignmentExpression((AssignmentExpressionSyntax)syntax),
            _ => throw new InvalidOperationException($"Unexpected syntax {syntax.Kind}")
        };
    }

    private BoundExpression BindNameExpression(NameExpressionSyntax syntax)
    {
        var name = syntax.IdentifierToken.Text;
        if (!_variables.TryGetValue(name, out var value))
        {
            _diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name);
            return new BoundLiteralExpression(0);
        }

        var type = value.GetType();
        return new BoundVariableExpression(name, type);
    }

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
    {
        var name = syntax.IdentifierToken.Text;
        var boundExpression = BindExpression(syntax.Expression);

        object defaultValue;
        if (boundExpression.Type == typeof(int))
        {
            defaultValue = 0;
        }
        else if (boundExpression.Type == typeof(bool))
        {
            defaultValue = false;
        }
        else
        {
            throw new InvalidOperationException($"Unsupported variable type '{boundExpression.Type}'");
        }

        _variables[name] = defaultValue;

        return new BoundAssignmentExpression(name, boundExpression);
    }

    private BoundExpression BindParenthesizedExpression(ParenthesizedExpressionSyntax syntax)
    {
        return BindExpression(syntax.Expression);
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var boundLeft = BindExpression(syntax.Left);
        var boundRight = BindExpression(syntax.Right);
        var boundOperator = BoundBinaryOperator.BindBinaryOperator(boundLeft, syntax.OperatorToken.Kind, boundRight);
        if (boundOperator != null)
        {
            return new BoundBinaryExpression(boundLeft, boundOperator, boundRight);
        }

        _diagnostics.ReportUndefinedBinaryOperator(
            syntax.OperatorToken.Span,
            syntax.OperatorToken,
            boundLeft.Type,
            boundRight.Type
        );
        return boundLeft;
    }

    private static BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        var value = syntax.Value;
        return new BoundLiteralExpression(value ?? 0);
    }

    private BoundExpression BindUnaryExpression(UnaryExpressionSyntax syntax)
    {
        var boundOperand = BindExpression(syntax.Operand);
        var boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand);
        if (boundOperator != null)
        {
            return new BoundUnaryExpression(boundOperator, boundOperand);
        }

        _diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken, boundOperand.Type);
        return boundOperand;
    }
}