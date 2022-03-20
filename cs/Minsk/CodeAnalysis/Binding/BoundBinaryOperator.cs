using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundBinaryOperator
{
    public Type LeftType { get; }
    public BoundBinaryOperatorKind OperatorKind { get; }
    public Type RightType { get; }
    public Type ResultType { get; }

    private BoundBinaryOperator(Type leftType, BoundBinaryOperatorKind operatorKind, Type rightType, Type resultType)
    {
        LeftType = leftType;
        OperatorKind = operatorKind;
        RightType = rightType;
        ResultType = resultType;
    }

    private static readonly BoundBinaryOperator[] KnownOperators =
    {
        new(typeof(int), BoundBinaryOperatorKind.Addition, typeof(int), typeof(int)),
        new(typeof(int), BoundBinaryOperatorKind.Subtraction, typeof(int), typeof(int)),
        new(typeof(int), BoundBinaryOperatorKind.Multiplication, typeof(int), typeof(int)),
        new(typeof(int), BoundBinaryOperatorKind.Division, typeof(int), typeof(int)),
    };

    public static BoundBinaryOperator? BindBinaryOperator(
        BoundExpression left,
        SyntaxKind operatorKind,
        BoundExpression right
    )
    {
        return KnownOperators.FirstOrDefault(knownOperator =>
            knownOperator.LeftType == left.Type && knownOperator.RightType == right.Type);
    }
}