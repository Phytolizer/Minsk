package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.VariableSymbol

internal class BoundScope(val parent: BoundScope?) {
    val _variables = mutableMapOf<String, VariableSymbol>()

    fun tryDeclare(variable: VariableSymbol): Boolean {
        return if (_variables.containsKey(variable.name)) {
            false
        } else {
            _variables[variable.name] = variable
            true
        }
    }

    fun tryLookup(name: String): VariableSymbol? {
        val variable = _variables[name]
        if (variable != null) {
            return variable
        }

        if (parent == null) {
            return null
        }

        return parent.tryLookup(name)
    }

    fun declaredVariables() = _variables.values.toList()
}
