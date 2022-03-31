namespace Minsk.CodeAnalysis.Syntax;

public class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(SyntaxToken elseKeyword, StatementSyntax elseStatement)
    {
        ElseKeyword = elseKeyword;
        ElseStatement = elseStatement;
    }

    public override SyntaxKind Kind => SyntaxKind.ElseClause;

    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[] { ElseKeyword, ElseStatement };

    public SyntaxToken ElseKeyword { get; }
    public StatementSyntax ElseStatement { get; }
}
