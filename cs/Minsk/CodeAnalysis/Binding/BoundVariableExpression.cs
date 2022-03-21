namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundVariableExpression : BoundExpression
{
    public VariableSymbol Variable { get; }
    public override BoundNodeKind Kind => BoundNodeKind.VariableExpression;
    public override Type Type => Variable.Type;

    public BoundVariableExpression(VariableSymbol variable)
    {
        Variable = variable;
    }
}