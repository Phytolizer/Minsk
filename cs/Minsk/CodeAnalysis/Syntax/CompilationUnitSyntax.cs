namespace Minsk.CodeAnalysis.Syntax;

public class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(ExpressionSyntax expression, SyntaxToken endOfFileToken)
    {
        Expression = expression;
        EndOfFileToken = endOfFileToken;
    }

    public ExpressionSyntax Expression { get; }
    public SyntaxToken EndOfFileToken { get; }
    public override SyntaxKind Kind => SyntaxKind.CompilationUnit;
    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[] { Expression, EndOfFileToken };
}
