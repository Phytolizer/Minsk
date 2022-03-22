package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.VariableSymbol
import kotlin.reflect.KClass

internal class BoundVariableExpression(val variable: VariableSymbol) : BoundExpression() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.VariableExpression
    override val type: KClass<out Any>
        get() = variable.type
}

