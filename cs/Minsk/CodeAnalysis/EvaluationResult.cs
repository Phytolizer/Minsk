using System.Collections.Immutable;

namespace Minsk.CodeAnalysis;

public sealed class EvaluationResult
{
    public readonly ImmutableArray<Diagnostic> Diagnostics;
    public readonly object? Value;

    internal EvaluationResult(ImmutableArray<Diagnostic> diagnostics, object? value)
    {
        Diagnostics = diagnostics;
        Value = value;
    }
}
