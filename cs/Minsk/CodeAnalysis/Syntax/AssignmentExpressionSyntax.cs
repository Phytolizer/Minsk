namespace Minsk.CodeAnalysis.Syntax;

public sealed class AssignmentExpressionSyntax : ExpressionSyntax
{
    public SyntaxToken IdentifierToken { get; }
    public SyntaxToken EqualsToken { get; }
    public ExpressionSyntax Expression { get; }
    public override SyntaxKind Kind => SyntaxKind.AssignmentExpression;

    protected override IEnumerable<SyntaxNode> Children =>
        new SyntaxNode[] { IdentifierToken, EqualsToken, Expression };

    public AssignmentExpressionSyntax(SyntaxToken identifierToken, SyntaxToken equalsToken, ExpressionSyntax expression)
    {
        IdentifierToken = identifierToken;
        EqualsToken = equalsToken;
        Expression = expression;
    }
}