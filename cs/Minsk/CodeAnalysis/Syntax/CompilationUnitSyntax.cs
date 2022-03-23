namespace Minsk.CodeAnalysis.Syntax;

public class CompilationUnitSyntax : SyntaxNode
{
    public CompilationUnitSyntax(StatementSyntax statement, SyntaxToken endOfFileToken)
    {
        Statement = statement;
        EndOfFileToken = endOfFileToken;
    }

    public StatementSyntax Statement { get; }
    public SyntaxToken EndOfFileToken { get; }
    public override SyntaxKind Kind => SyntaxKind.CompilationUnit;
    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[] { Statement, EndOfFileToken };
}
