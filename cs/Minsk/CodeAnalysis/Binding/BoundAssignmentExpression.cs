namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundAssignmentExpression : BoundExpression
{
    public VariableSymbol Variable { get; }
    public BoundExpression Expression { get; }
    public override BoundNodeKind Kind => BoundNodeKind.AssignmentExpression;
    public override Type Type => Expression.Type;

    public BoundAssignmentExpression(VariableSymbol variable, BoundExpression expression)
    {
        Variable = variable;
        Expression = expression;
    }
}