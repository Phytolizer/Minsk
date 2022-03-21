using Minsk.CodeAnalysis.Binding;
using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis;

public class Compilation
{
    private readonly SyntaxTree _syntaxTree;

    public Compilation(SyntaxTree syntaxTree)
    {
        _syntaxTree = syntaxTree;
    }

    public EvaluationResult Evaluate()
    {
        var binder = new Binder();
        var boundExpression = binder.BindExpression(_syntaxTree.Root);
        var diagnostics = _syntaxTree.Diagnostics.Concat(binder.Diagnostics).ToArray();
        if (diagnostics.Any())
        {
            return new EvaluationResult(diagnostics, null);
        }

        var evaluator = new Evaluator(boundExpression);
        return new EvaluationResult(Array.Empty<string>(), evaluator.Evaluate());
    }
}