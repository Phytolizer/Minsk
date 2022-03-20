using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal class BoundUnaryOperator
{
    public BoundUnaryOperatorKind OperatorKind { get; }
    public SyntaxKind SyntaxKind { get; }
    public Type OperandType { get; }
    public Type ResultType { get; }

    private BoundUnaryOperator(SyntaxKind syntaxKind, BoundUnaryOperatorKind operatorKind, Type operandType,
        Type resultType)
    {
        SyntaxKind = syntaxKind;
        OperatorKind = operatorKind;
        OperandType = operandType;
        ResultType = resultType;
    }

    private static readonly BoundUnaryOperator[] KnownOperators =
    {
        new(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, typeof(int), typeof(int)),
        new(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, typeof(int), typeof(int)),
    };

    public static BoundUnaryOperator? Bind(SyntaxKind operatorKind, BoundExpression operand)
    {
        return KnownOperators.FirstOrDefault(op => op.SyntaxKind == operatorKind && op.OperandType == operand.Type);
    }
}