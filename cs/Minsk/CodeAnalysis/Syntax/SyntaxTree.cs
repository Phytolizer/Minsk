namespace Minsk.CodeAnalysis.Syntax;

public sealed class SyntaxTree
{
    internal SyntaxTree(ExpressionSyntax root, SyntaxToken endOfFileToken, Diagnostic[] diagnostics)
    {
        Root = root;
        EndOfFileToken = endOfFileToken;
        Diagnostics = diagnostics;
    }

    public ExpressionSyntax Root { get; }
    public SyntaxToken EndOfFileToken { get; }
    public Diagnostic[] Diagnostics { get; }

    public static SyntaxTree Parse(string text)
    {
        var parser = new Parser(text);
        return parser.Parse();
    }
}