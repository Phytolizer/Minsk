using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundBinaryOperator
{
    private static readonly BoundBinaryOperator[] KnownOperators =
    {
        new(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, typeof(int), typeof(int), typeof(int)),
        new(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, typeof(int), typeof(int), typeof(int)),
        new(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, typeof(int), typeof(int), typeof(int)),
        new(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, typeof(int), typeof(int), typeof(int)),
        new(
            SyntaxKind.AmpersandAmpersandToken,
            BoundBinaryOperatorKind.LogicalAnd,
            typeof(bool),
            typeof(bool),
            typeof(bool)
        ),
        new(
            SyntaxKind.PipePipeToken,
            BoundBinaryOperatorKind.LogicalOr,
            typeof(bool),
            typeof(bool),
            typeof(bool)
        ),
        new(
            SyntaxKind.EqualsEqualsToken,
            BoundBinaryOperatorKind.Equality,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
        new(
            SyntaxKind.BangEqualsToken,
            BoundBinaryOperatorKind.Inequality,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
        new(
            SyntaxKind.EqualsEqualsToken,
            BoundBinaryOperatorKind.Equality,
            typeof(bool),
            typeof(bool),
            typeof(bool)
        ),
        new(
            SyntaxKind.BangEqualsToken,
            BoundBinaryOperatorKind.Inequality,
            typeof(bool),
            typeof(bool),
            typeof(bool)
        ),
        new(
            SyntaxKind.LessToken,
            BoundBinaryOperatorKind.LessThan,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
        new(
            SyntaxKind.LessEqualsToken,
            BoundBinaryOperatorKind.LessThanOrEqual,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
        new(
            SyntaxKind.GreaterToken,
            BoundBinaryOperatorKind.GreaterThan,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
        new(
            SyntaxKind.GreaterEqualsToken,
            BoundBinaryOperatorKind.GreaterThanOrEqual,
            typeof(int),
            typeof(int),
            typeof(bool)
        ),
    };

    private BoundBinaryOperator(
        SyntaxKind syntaxKind,
        BoundBinaryOperatorKind operatorKind,
        Type leftType,
        Type rightType,
        Type resultType
    )
    {
        SyntaxKind = syntaxKind;
        OperatorKind = operatorKind;
        LeftType = leftType;
        RightType = rightType;
        ResultType = resultType;
    }

    private Type LeftType { get; }
    private SyntaxKind SyntaxKind { get; }
    public BoundBinaryOperatorKind OperatorKind { get; }
    private Type RightType { get; }
    public Type ResultType { get; }

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
