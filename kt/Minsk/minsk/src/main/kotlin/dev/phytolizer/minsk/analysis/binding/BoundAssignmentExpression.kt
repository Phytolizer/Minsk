package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal class BoundAssignmentExpression(val name: String, val expression: BoundExpression) : BoundExpression() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.AssignmentExpression
    override val type: KClass<out Any>
        get() = expression.type
}
