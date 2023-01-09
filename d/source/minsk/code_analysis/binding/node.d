module minsk.code_analysis.binding.node;

import minsk.runtime.object : Obj, Type;

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
}

final class BoundBinaryExpression : BoundExpression {
    private const(BoundExpression) _left;
    private BoundBinaryOperatorKind _operatorKind;
    private const(BoundExpression) _right;

    this(
        const(BoundExpression) left,
        BoundBinaryOperatorKind operatorKind,
        const(BoundExpression) right
    ) {
        _left = left;
        _operatorKind = operatorKind;
        _right = right;
    }

    const(BoundExpression) left() const {
        return _left;
    }

    BoundBinaryOperatorKind operatorKind() const {
        return _operatorKind;
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

final class BoundUnaryExpression : BoundExpression {
    private BoundUnaryOperatorKind _operatorKind;
    private const(BoundExpression) _operand;

    this(
        BoundUnaryOperatorKind operatorKind,
        const(BoundExpression) operand
    ) {
        _operatorKind = operatorKind;
        _operand = operand;
    }

    BoundUnaryOperatorKind operatorKind() const {
        return _operatorKind;
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
