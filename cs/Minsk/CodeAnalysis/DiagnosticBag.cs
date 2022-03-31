using System.Collections;
using Minsk.CodeAnalysis.Syntax;
using Minsk.CodeAnalysis.Text;

namespace Minsk.CodeAnalysis;

internal sealed class DiagnosticBag : IEnumerable<Diagnostic>
{
    private readonly List<Diagnostic> _diagnostics = new();

    public IEnumerator<Diagnostic> GetEnumerator()
    {
        return _diagnostics.GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    private void Report(TextSpan span, string message)
    {
        var diagnostic = new Diagnostic(span, message);
        _diagnostics.Add(diagnostic);
    }

    public void ReportBadCharacter(int position, char c)
    {
        var message = $"Bad character in input: {c}";
        var span = new TextSpan(position, 1);
        Report(span, message);
    }

    public void AddRange(IEnumerable<Diagnostic> diagnostics)
    {
        _diagnostics.AddRange(diagnostics);
    }

    public void ReportInvalidInt(TextSpan span, string text, Type type)
    {
        var message = $"The number {text} doesn't fit in {type}";
        Report(span, message);
    }

    public void ReportUnexpectedToken(TextSpan span, SyntaxKind expectedKind, SyntaxKind actualKind)
    {
        var message = $"Expected next token to be {expectedKind}, got {actualKind} instead";
        Report(span, message);
    }

    public void ReportUndefinedBinaryOperator(TextSpan span, SyntaxToken op, Type leftType, Type rightType)
    {
        var message = $"Binary operator '{op.Text}' is not defined for types '{leftType}' and '{rightType}'";
        Report(span, message);
    }

    public void ReportUndefinedUnaryOperator(TextSpan span, SyntaxToken op, Type operandType)
    {
        var message = $"Unary operator '{op.Text}' is not defined for type '{operandType}'";
        Report(span, message);
    }

    public void ReportUndefinedName(TextSpan span, string name)
    {
        var message = $"Undefined name '{name}'";
        Report(span, message);
    }

    public void ReportVariableAlreadyDeclared(TextSpan span, string name)
    {
        var message = $"Name '{name}' is already declared in this scope";
        Report(span, message);
    }

    public void ReportCannotConvert(TextSpan span, Type fromType, Type toType)
    {
        var message = $"Cannot convert type '{fromType}' to '{toType}'";
        Report(span, message);
    }

    public void ReportCannotAssign(TextSpan span, string name)
    {
        var message = $"Cannot assign to read-only variable '{name}'";
        Report(span, message);
    }
}
