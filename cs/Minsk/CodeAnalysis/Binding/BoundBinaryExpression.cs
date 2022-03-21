namespace Minsk.CodeAnalysis.Binding;

internal class BoundBinaryExpression : BoundExpression
{
    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator op, BoundExpression right)
    {
        Left = left;
        Op = op;
        Right = right;
    }

    public BoundExpression Left { get; }
    public BoundBinaryOperator Op { get; }
    public BoundExpression Right { get; }
    public override Type Type => Op.ResultType;
    public override BoundNodeKind Kind => BoundNodeKind.BinaryExpression;
}