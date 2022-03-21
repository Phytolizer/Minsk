using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundBinaryOperator
{
    public Type LeftType { get; }
    public SyntaxKind SyntaxKind { get; }
    public BoundBinaryOperatorKind OperatorKind { get; }
    public Type RightType { get; }
    public Type ResultType { get; }

    private BoundBinaryOperator(Type leftType, SyntaxKind syntaxKind, BoundBinaryOperatorKind operatorKind,
        Type rightType, Type resultType)
    {
        LeftType = leftType;
        SyntaxKind = syntaxKind;
        OperatorKind = operatorKind;
        RightType = rightType;
        ResultType = resultType;
    }

    private static readonly BoundBinaryOperator[] KnownOperators =
    {
        new(typeof(int), SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, typeof(int), typeof(int)),
        new(typeof(int), SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, typeof(int), typeof(int)),
        new(typeof(int), SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, typeof(int), typeof(int)),
        new(typeof(int), SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, typeof(int), typeof(int)),
        new(
            typeof(bool),
            SyntaxKind.AmpersandAmpersandToken,
            BoundBinaryOperatorKind.LogicalAnd,
            typeof(bool),
            typeof(bool)
        ),
        new(
            typeof(bool),
            SyntaxKind.PipePipeToken,
            BoundBinaryOperatorKind.LogicalOr,
            typeof(bool),
            typeof(bool)
        ),
    };

    public static BoundBinaryOperator? BindBinaryOperator(
        BoundExpression left,
        SyntaxKind operatorKind,
        BoundExpression right
    )
    {
        return KnownOperators.FirstOrDefault(knownOperator =>
            knownOperator.LeftType == left.Type && knownOperator.SyntaxKind == operatorKind &&
            knownOperator.RightType == right.Type);
    }
}