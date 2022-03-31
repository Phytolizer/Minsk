namespace Minsk.CodeAnalysis.Syntax;

public sealed class IfStatementSyntax : StatementSyntax
{
    public IfStatementSyntax(SyntaxToken ifKeyword, ExpressionSyntax condition, StatementSyntax thenStatement, ElseClauseSyntax? elseClause)
    {
        IfKeyword = ifKeyword;
        Condition = condition;
        ThenStatement = thenStatement;
        ElseClause = elseClause;
    }

    public override SyntaxKind Kind => SyntaxKind.IfStatement;

    public override IEnumerable<SyntaxNode> Children
    {
        get
        {
            yield return IfKeyword;
            yield return Condition;
            yield return ThenStatement;
            if (ElseClause != null)
            {
                yield return ElseClause;
            }
        }
    }

    public SyntaxToken IfKeyword { get; }
    public ExpressionSyntax Condition { get; }
    public StatementSyntax ThenStatement { get; }
    public ElseClauseSyntax? ElseClause { get; }
}
