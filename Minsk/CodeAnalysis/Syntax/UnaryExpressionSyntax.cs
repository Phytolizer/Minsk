namespace Minsk.CodeAnalysis.Syntax;

public class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(SyntaxToken operatorToken, ExpressionSyntax right)
    {
        OperatorToken = operatorToken;
        Right = right;
    }

    public override SyntaxKind Kind => SyntaxKind.UnaryExpression;

    public override IEnumerable<SyntaxNode> Children
    {
        get
        {
            yield return OperatorToken;
            yield return Right;
        }
    }

    public SyntaxToken OperatorToken { get; }
    public ExpressionSyntax Right { get; }
}