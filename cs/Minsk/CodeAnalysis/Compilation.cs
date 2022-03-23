using System.Collections.Immutable;
using Minsk.CodeAnalysis.Binding;
using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis;

public sealed class Compilation
{
    private readonly SyntaxTree _syntaxTree;

    public Compilation(SyntaxTree syntaxTree)
    {
        _syntaxTree = syntaxTree;
    }

    public EvaluationResult Evaluate(Dictionary<VariableSymbol, object> variables)
    {
        var globalScope = Binder.BindGlobalScope(_syntaxTree.Root);
        var diagnostics = _syntaxTree.Diagnostics.Concat(globalScope.Diagnostics).ToImmutableArray();
        if (diagnostics.Any())
        {
            return new EvaluationResult(diagnostics, null);
        }

        var evaluator = new Evaluator(globalScope.Expression, variables);
        return new EvaluationResult(ImmutableArray<Diagnostic>.Empty, evaluator.Evaluate());
    }
}
