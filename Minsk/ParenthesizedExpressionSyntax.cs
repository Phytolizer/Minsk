namespace Minsk;

public class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public ParenthesizedExpressionSyntax(SyntaxToken openParenthesisToken, ExpressionSyntax expression,
        SyntaxToken closeParenthesisToken)
    {
        Expression = expression;
        CloseParenthesisToken = closeParenthesisToken;
        OpenParenthesisToken = openParenthesisToken;
    }

    public override SyntaxKind Kind => SyntaxKind.ParenthesizedExpression;

    public override IEnumerable<SyntaxNode> Children
    {
        get
        {
            yield return OpenParenthesisToken;
            yield return Expression;
            yield return CloseParenthesisToken;
        }
    }

    public SyntaxToken OpenParenthesisToken { get; }
    public ExpressionSyntax Expression { get; }
    public SyntaxToken CloseParenthesisToken { get; }
}