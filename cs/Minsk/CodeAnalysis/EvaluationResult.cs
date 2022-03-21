namespace Minsk.CodeAnalysis;

public sealed class EvaluationResult
{
    public readonly Diagnostic[] Diagnostics;
    public readonly object? Value;

    internal EvaluationResult(Diagnostic[] diagnostics, object? value)
    {
        Diagnostics = diagnostics;
        Value = value;
    }
}