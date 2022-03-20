namespace Minsk.CodeAnalysis.Binding;

internal class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }
    public override Type Type => Value.GetType();
    public override BoundNodeKind Kind => BoundNodeKind.LiteralExpression;

    public BoundLiteralExpression(object value)
    {
        Value = value;
    }
}