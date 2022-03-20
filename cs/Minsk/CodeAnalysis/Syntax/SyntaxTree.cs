using Minsk.CodeAnalysis.Binding;

namespace Minsk.CodeAnalysis.Syntax;

public sealed class SyntaxTree
{
    public ExpressionSyntax Root { get; }
    public SyntaxToken EndOfFileToken { get; }
    public string[] Diagnostics { get; }

    internal SyntaxTree(ExpressionSyntax root, SyntaxToken endOfFileToken, string[] diagnostics)
    {
        Root = root;
        EndOfFileToken = endOfFileToken;
        Diagnostics = diagnostics;
    }

    public static SyntaxTree Parse(string text)
    {
        var parser = new Parser(text);
        return parser.Parse();
    }

    public int Evaluate()
    {
        var boundExpression = new Binder().BindExpression(Root);
        return new Evaluator(boundExpression).Evaluate();
    }
}