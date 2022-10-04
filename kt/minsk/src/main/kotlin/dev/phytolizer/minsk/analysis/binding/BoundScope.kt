package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.VariableSymbol

internal class BoundScope(val parent: BoundScope?) {
    private val variables = mutableMapOf<String, VariableSymbol>()

    fun tryDeclare(variable: VariableSymbol): Boolean {
        return if (variables.containsKey(variable.name)) {
            false
        } else {
            variables[variable.name] = variable
            true
        }
    }

    fun tryLookup(name: String): VariableSymbol? {
        val variable = variables[name]
        if (variable != null) {
            return variable
        }

        if (parent == null) {
            return null
        }

        return parent.tryLookup(name)
    }

    fun declaredVariables() = variables.values.toList()
}
