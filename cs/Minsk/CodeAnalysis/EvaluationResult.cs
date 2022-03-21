namespace Minsk.CodeAnalysis;

public sealed class EvaluationResult
{
    public object? Value;
    public string[] Diagnostics;

    internal EvaluationResult(string[] diagnostics, object? value)
    {
        Diagnostics = diagnostics;
        Value = value;
    }
}