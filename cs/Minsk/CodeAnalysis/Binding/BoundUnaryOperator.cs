using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal class BoundUnaryOperator
{
    public BoundUnaryOperatorKind OperatorKind { get; }
    public Type OperandType { get; }
    public Type ResultType { get; }

    private BoundUnaryOperator(BoundUnaryOperatorKind operatorKind, Type operandType, Type resultType)
    {
        OperatorKind = operatorKind;
        OperandType = operandType;
        ResultType = resultType;
    }

    private static readonly BoundUnaryOperator[] KnownOperators =
    {
        new(BoundUnaryOperatorKind.Identity, typeof(int), typeof(int)),
        new(BoundUnaryOperatorKind.Negation, typeof(int), typeof(int)),
    };

    public static BoundUnaryOperator? Bind(SyntaxKind operatorKind, BoundExpression operand)
    {
        return KnownOperators.FirstOrDefault(op => op.OperandType == operand.Type);
    }
}