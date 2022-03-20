namespace Minsk.CodeAnalysis;

public class SyntaxTree
{
    public ExpressionSyntax Root { get; }
    public SyntaxToken EndOfFileToken { get; }
    public string[] Diagnostics { get; }

    public SyntaxTree(ExpressionSyntax root, SyntaxToken endOfFileToken, string[] diagnostics)
    {
        Root = root;
        EndOfFileToken = endOfFileToken;
        Diagnostics = diagnostics;
    }
}