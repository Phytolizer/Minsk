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
        var binder = new Binder(variables);
        var boundExpression = binder.BindExpression(_syntaxTree.Root);
        var diagnostics = _syntaxTree.Diagnostics.Concat(binder.Diagnostics).ToArray();
        if (diagnostics.Any())
        {
            return new EvaluationResult(diagnostics, null);
        }

        var evaluator = new Evaluator(boundExpression, variables);
        return new EvaluationResult(Array.Empty<Diagnostic>(), evaluator.Evaluate());
    }
}
