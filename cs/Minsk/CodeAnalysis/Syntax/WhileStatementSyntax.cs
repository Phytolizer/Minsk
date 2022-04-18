namespace Minsk.CodeAnalysis.Syntax;

public sealed class WhileStatementSyntax : StatementSyntax
{
    public WhileStatementSyntax(SyntaxToken keywordToken, ExpressionSyntax condition, StatementSyntax body)
    {
        KeywordToken = keywordToken;
        Condition = condition;
        Body = body;
    }

    public override SyntaxKind Kind => SyntaxKind.WhileStatement;

    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[] { KeywordToken, Condition, Body };

    public SyntaxToken KeywordToken { get; }
    public ExpressionSyntax Condition { get; }
    public StatementSyntax Body { get; }
}
