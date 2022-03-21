namespace Minsk.CodeAnalysis.Syntax;

public sealed class EvaluationResult
{
    public int Value;
    public string[] Diagnostics;

    public EvaluationResult(int value)
    {
        Value = value;
        Diagnostics = Array.Empty<string>();
    }

    public EvaluationResult(IEnumerable<string> diagnostics)
    {
        Diagnostics = diagnostics as string[] ?? diagnostics.ToArray();
    }
}