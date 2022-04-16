namespace Minsk.CodeAnalysis.Syntax;

public sealed class ForStatementSyntax : StatementSyntax
{
    public ForStatementSyntax(SyntaxToken forKeyword, SyntaxToken identifierToken, SyntaxToken equalsToken,
            ExpressionSyntax lowerBound, SyntaxToken toKeyword, ExpressionSyntax upperBound, StatementSyntax body)
    {
        ForKeyword = forKeyword;
        IdentifierToken = identifierToken;
        EqualsToken = equalsToken;
        LowerBound = lowerBound;
        ToKeyword = toKeyword;
        UpperBound = upperBound;
        Body = body;
    }

    public SyntaxToken ForKeyword { get; }
    public SyntaxToken IdentifierToken { get; }
    public SyntaxToken EqualsToken { get; }
    public ExpressionSyntax LowerBound { get; }
    public SyntaxToken ToKeyword { get; }
    public ExpressionSyntax UpperBound { get; }
    public StatementSyntax Body { get; }

    public override SyntaxKind Kind => SyntaxKind.ForStatement;

    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[]
    {
        ForKeyword,
        IdentifierToken,
        EqualsToken,
        LowerBound,
        ToKeyword,
        UpperBound,
        Body
    };
}
