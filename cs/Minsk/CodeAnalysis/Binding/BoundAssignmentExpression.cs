namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundAssignmentExpression : BoundExpression
{
    public string Name { get; }
    public BoundExpression Expression { get; }
    public override BoundNodeKind Kind => BoundNodeKind.AssignmentExpression;
    public override Type Type => Expression.Type;

    public BoundAssignmentExpression(string name, BoundExpression expression)
    {
        Name = name;
        Expression = expression;
    }
}