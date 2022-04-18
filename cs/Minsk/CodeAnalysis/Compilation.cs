using System.Collections.Immutable;
using Minsk.CodeAnalysis.Binding;
using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis;

public sealed class Compilation
{
    public Compilation? Previous { get; }
    public SyntaxTree SyntaxTree { get; }
    private BoundGlobalScope? _globalScope;

    public Compilation(SyntaxTree syntaxTree)
    {
        SyntaxTree = syntaxTree;
    }

    private Compilation(Compilation previous, SyntaxTree syntaxTree)
    {
        Previous = previous;
        SyntaxTree = syntaxTree;
    }

    internal BoundGlobalScope GlobalScope
    {
        get
        {
            if (_globalScope != null)
            {
                return _globalScope;
            }

            var globalScope = Binder.BindGlobalScope(Previous?.GlobalScope, SyntaxTree.Root);
            Interlocked.CompareExchange(ref _globalScope, globalScope, null);

            return _globalScope;
        }
    }

    public Compilation ContinueWith(SyntaxTree syntaxTree)
    {
        return new Compilation(this, syntaxTree);
    }

    public EvaluationResult Evaluate(Dictionary<VariableSymbol, object> variables)
    {
        var diagnostics = SyntaxTree.Diagnostics.Concat(GlobalScope.Diagnostics).ToImmutableArray();
        if (diagnostics.Any())
        {
            return new EvaluationResult(diagnostics, null);
        }

        var evaluator = new Evaluator(GlobalScope.Statement, variables);
        return new EvaluationResult(ImmutableArray<Diagnostic>.Empty, evaluator.Evaluate());
    }
}
