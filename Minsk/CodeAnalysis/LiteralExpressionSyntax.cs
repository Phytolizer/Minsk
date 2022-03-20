namespace Minsk.CodeAnalysis;

public class LiteralExpressionSyntax : ExpressionSyntax
{
    public SyntaxToken LiteralToken { get; }

    public override SyntaxKind Kind => SyntaxKind.LiteralExpression;
    public override IEnumerable<SyntaxNode> Children => new[] { LiteralToken };

    public LiteralExpressionSyntax(SyntaxToken literalToken)
    {
        LiteralToken = literalToken;
    }
}