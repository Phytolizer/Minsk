package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal class BoundUnaryExpression(val op: BoundUnaryOperator, val operand: BoundExpression) :
    BoundExpression() {

    override val kind: BoundNodeKind
        get() = BoundNodeKind.UnaryExpression
    override val type: KClass<out Any>
        get() = op.resultType
}
