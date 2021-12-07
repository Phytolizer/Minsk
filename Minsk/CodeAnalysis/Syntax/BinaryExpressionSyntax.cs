namespace Minsk.CodeAnalysis.Syntax;

public class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(ExpressionSyntax left, SyntaxToken operatorToken, ExpressionSyntax right)
    {
        Left = left;
        OperatorToken = operatorToken;
        Right = right;
    }

    public override SyntaxKind Kind => SyntaxKind.BinaryExpression;

    public override IEnumerable<SyntaxNode> Children
    {
        get
        {
            yield return Left;
            yield return OperatorToken;
            yield return Right;
        }
    }

    public SyntaxToken OperatorToken { get; }
    public ExpressionSyntax Left { get; }
    public ExpressionSyntax Right { get; }
}