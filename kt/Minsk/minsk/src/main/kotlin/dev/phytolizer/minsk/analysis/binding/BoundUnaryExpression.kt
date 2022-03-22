package dev.phytolizer.minsk.analysis.binding

import java.lang.reflect.Type

internal class BoundUnaryExpression(val op: BoundUnaryOperator, val operand: BoundExpression) :
    BoundExpression() {

    override val kind: BoundNodeKind
        get() = BoundNodeKind.UnaryExpression
    override val type: Type
        get() = op.resultType
}
