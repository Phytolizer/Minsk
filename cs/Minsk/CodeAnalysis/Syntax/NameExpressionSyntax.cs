namespace Minsk.CodeAnalysis.Syntax;

public sealed class NameExpressionSyntax : ExpressionSyntax
{
    public SyntaxToken IdentifierToken { get; }
    public override SyntaxKind Kind => SyntaxKind.NameExpression;
    protected override IEnumerable<SyntaxNode> Children => new[] { IdentifierToken };

    public NameExpressionSyntax(SyntaxToken identifierToken)
    {
        IdentifierToken = identifierToken;
    }
}