package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal class BoundVariableExpression(val name: String, override val type: KClass<out Any>) : BoundExpression() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.VariableExpression
}

