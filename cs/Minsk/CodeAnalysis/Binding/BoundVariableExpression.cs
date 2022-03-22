namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundVariableExpression : BoundExpression
{
    public BoundVariableExpression(VariableSymbol variable)
    {
        Variable = variable;
    }

    public VariableSymbol Variable { get; }
    public override BoundNodeKind Kind => BoundNodeKind.VariableExpression;
    public override Type Type => Variable.Type;
}
