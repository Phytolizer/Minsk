package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.VariableSymbol
import kotlin.reflect.KClass

internal class BoundAssignmentExpression(val variable: VariableSymbol, val expression: BoundExpression) :
    BoundExpression() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.AssignmentExpression
    override val type: KClass<out Any>
        get() = variable.type
}
