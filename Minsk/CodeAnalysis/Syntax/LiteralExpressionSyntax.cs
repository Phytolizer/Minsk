namespace Minsk.CodeAnalysis.Syntax;

public class LiteralExpressionSyntax : ExpressionSyntax
{
    public LiteralExpressionSyntax(SyntaxToken literalToken)
    {
        LiteralToken = literalToken;
    }

    public override SyntaxKind Kind => SyntaxKind.LiteralExpression;

    public override IEnumerable<SyntaxNode> Children
    {
        get { yield return LiteralToken; }
    }

    public SyntaxToken LiteralToken { get; }
}