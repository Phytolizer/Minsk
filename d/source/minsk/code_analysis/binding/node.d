module minsk.code_analysis.binding.node;

import minsk.code_analysis.syntax : SyntaxKind;
import minsk.runtime.object : Obj, Type;

import optional : Optional, some, no;

enum BoundNodeKind {
    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
}

interface BoundNode {
    BoundNodeKind kind() const;
}

interface BoundExpression : BoundNode {
    Type type() const;
}

final class BoundLiteralExpression : BoundExpression {
    private const(Obj) _value;

    this(const(Obj) value) {
        _value = value;
    }

    const(Obj) value() const {
        return _value;
    }

    override BoundNodeKind kind() const {
        return BoundNodeKind.LiteralExpression;
    }

    override Type type() const {
        return _value.type;
    }
}

enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equality,
    Inequality,
}

struct BoundBinaryOperator {
    SyntaxKind syntaxKind;
    BoundBinaryOperatorKind kind;
    Type leftType;
    Type rightType;
    Type resultType;

    this(
        SyntaxKind syntaxKind,
        BoundBinaryOperatorKind kind,
        Type leftType,
        Type rightType,
        Type resultType,
    ) {
        this.syntaxKind = syntaxKind;
        this.kind = kind;
        this.leftType = leftType;
        this.rightType = rightType;
        this.resultType = resultType;
    }

    this(SyntaxKind syntaxKind, BoundBinaryOperatorKind kind, Type type) {
        this(syntaxKind, kind, type, type, type);
    }

    this(
        SyntaxKind syntaxKind,
        BoundBinaryOperatorKind kind,
        Type operandType,
        Type resultType,
    ) {
        this(syntaxKind, kind, operandType, operandType, resultType);
    }
}

private immutable binaryOperators = [
    BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, Type.Integer),
    BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, Type.Integer),
    BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, Type.Integer),
    BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, Type.Integer),
    BoundBinaryOperator(SyntaxKind.AmpersandAmpersandToken, BoundBinaryOperatorKind.LogicalAnd, Type.Boolean),
    BoundBinaryOperator(SyntaxKind.PipePipeToken, BoundBinaryOperatorKind.LogicalOr, Type.Boolean),
    BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equality, Type.Integer),
    BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equality, Type.Boolean),
    BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.Inequality, Type.Integer),
    BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.Inequality, Type.Boolean),
];

Optional!(immutable(BoundBinaryOperator)) bindBinaryOperator(SyntaxKind kind, Type leftType, Type rightType) {
    foreach (bo; binaryOperators) {
        if (bo.syntaxKind == kind && bo.leftType == leftType && bo.rightType == rightType) {
            return some(bo);
        }
    }
    return no!(immutable(BoundBinaryOperator));
}

final class BoundBinaryExpression : BoundExpression {
    private const(BoundExpression) _left;
    private immutable(BoundBinaryOperator) _operator;
    private const(BoundExpression) _right;

    this(
        const(BoundExpression) left,
        immutable(BoundBinaryOperator) operator,
        const(BoundExpression) right
    ) {
        _left = left;
        _operator = operator;
        _right = right;
    }

    const(BoundExpression) left() const {
        return _left;
    }

    immutable(BoundBinaryOperator) operator() const {
        return _operator;
    }

    const(BoundExpression) right() const {
        return _right;
    }

    override BoundNodeKind kind() const {
        return BoundNodeKind.BinaryExpression;
    }

    override Type type() const {
        return _left.type;
    }
}

enum BoundUnaryOperatorKind {
    Identity,
    ArithmeticNegation,
    LogicalNegation,
}

struct BoundUnaryOperator {
    SyntaxKind syntaxKind;
    BoundUnaryOperatorKind kind;
    Type operandType;
    Type resultType;

    this(
        SyntaxKind syntaxKind,
        BoundUnaryOperatorKind kind,
        Type operandType,
        Type resultType,
    ) {
        this.syntaxKind = syntaxKind;
        this.kind = kind;
        this.operandType = operandType;
        this.resultType = resultType;
    }

    this(SyntaxKind syntaxKind, BoundUnaryOperatorKind kind, Type type) {
        this(syntaxKind, kind, type, type);
    }
}

private immutable unaryOperators = [
    BoundUnaryOperator(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, Type.Integer),
    BoundUnaryOperator(SyntaxKind.MinusToken, BoundUnaryOperatorKind.ArithmeticNegation, Type.Integer),
    BoundUnaryOperator(SyntaxKind.BangToken, BoundUnaryOperatorKind.LogicalNegation, Type.Boolean),
];

Optional!(immutable(BoundUnaryOperator)) bindUnaryOperator(SyntaxKind kind, Type operandType) {
    foreach (uo; unaryOperators) {
        if (uo.syntaxKind == kind && uo.operandType == operandType) {
            return some(uo);
        }
    }
    return no!(immutable(BoundUnaryOperator));
}

final class BoundUnaryExpression : BoundExpression {
    private immutable(BoundUnaryOperator) _operator;
    private const(BoundExpression) _operand;

    this(
        immutable(BoundUnaryOperator) operator,
        const(BoundExpression) operand
    ) {
        _operator = operator;
        _operand = operand;
    }

    immutable(BoundUnaryOperator) operator() const {
        return _operator;
    }

    const(BoundExpression) operand() const {
        return _operand;
    }

    override BoundNodeKind kind() const {
        return BoundNodeKind.UnaryExpression;
    }

    override Type type() const {
        return _operand.type;
    }
}
