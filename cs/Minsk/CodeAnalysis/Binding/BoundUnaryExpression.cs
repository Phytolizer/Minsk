using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal class BoundUnaryExpression : BoundExpression
{
    public BoundUnaryOperator Op { get; }
    public BoundExpression Operand { get; }
    public override Type Type => Op.ResultType;
    public override BoundNodeKind Kind => BoundNodeKind.UnaryExpression;

    public BoundUnaryExpression(BoundUnaryOperator op, BoundExpression operand)
    {
        Op = op;
        Operand = operand;
    }
}