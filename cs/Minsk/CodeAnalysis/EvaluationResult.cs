namespace Minsk.CodeAnalysis;

public sealed class EvaluationResult
{
    public Diagnostic[] Diagnostics;
    public object? Value;

    internal EvaluationResult(Diagnostic[] diagnostics, object? value)
    {
        Diagnostics = diagnostics;
        Value = value;
    }
}