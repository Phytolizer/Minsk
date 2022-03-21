namespace Minsk.CodeAnalysis.Syntax;

public sealed class UnaryExpressionSyntax : ExpressionSyntax
{
    public UnaryExpressionSyntax(SyntaxToken operatorToken, ExpressionSyntax operand)
    {
        OperatorToken = operatorToken;
        Operand = operand;
    }

    public SyntaxToken OperatorToken { get; }
    public ExpressionSyntax Operand { get; }

    public override SyntaxKind Kind => SyntaxKind.UnaryExpression;
    protected override IEnumerable<SyntaxNode> Children => new SyntaxNode[] { OperatorToken, Operand };
}