using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class BoundUnaryOperator
{
    private static readonly BoundUnaryOperator[] KnownOperators =
    {
        new(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, typeof(int), typeof(int)),
        new(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, typeof(int), typeof(int)),
        new(SyntaxKind.BangToken, BoundUnaryOperatorKind.LogicalNegation, typeof(bool), typeof(bool)),
        new(SyntaxKind.TildeToken, BoundUnaryOperatorKind.BitwiseNegation, typeof(int), typeof(int))
    };

    private BoundUnaryOperator(
        SyntaxKind syntaxKind,
        BoundUnaryOperatorKind operatorKind,
        Type operandType,
        Type resultType
    )
    {
        SyntaxKind = syntaxKind;
        OperatorKind = operatorKind;
        OperandType = operandType;
        ResultType = resultType;
    }

    public BoundUnaryOperatorKind OperatorKind { get; }
    private SyntaxKind SyntaxKind { get; }
    private Type OperandType { get; }
    public Type ResultType { get; }

    public static BoundUnaryOperator? Bind(SyntaxKind operatorKind, BoundExpression operand)
    {
        return KnownOperators.FirstOrDefault(op => op.SyntaxKind == operatorKind && op.OperandType == operand.Type);
    }
}
