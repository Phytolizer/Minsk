namespace Minsk.CodeAnalysis.Syntax;

public sealed class EvaluationResult
{
    public object? Value;
    public string[] Diagnostics;

    public EvaluationResult(object value)
    {
        Value = value;
        Diagnostics = Array.Empty<string>();
    }

    public EvaluationResult(IEnumerable<string> diagnostics)
    {
        Diagnostics = diagnostics as string[] ?? diagnostics.ToArray();
    }
}