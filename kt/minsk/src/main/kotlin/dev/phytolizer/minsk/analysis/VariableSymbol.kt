package dev.phytolizer.minsk.analysis

import kotlin.reflect.KClass

class VariableSymbol(val name: String, val isReadOnly: Boolean, val type: KClass<out Any>)
