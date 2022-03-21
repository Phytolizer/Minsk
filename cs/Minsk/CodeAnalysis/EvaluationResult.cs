namespace Minsk.CodeAnalysis;

public sealed class EvaluationResult
{
    public object? Value;
    public string[] Diagnostics;

    public EvaluationResult(string[] diagnostics, object? value)
    {
        Diagnostics = diagnostics;
        Value = value;
    }
}