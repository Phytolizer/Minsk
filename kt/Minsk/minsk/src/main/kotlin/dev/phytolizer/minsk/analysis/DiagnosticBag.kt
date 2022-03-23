package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.syntax.SyntaxKind
import dev.phytolizer.minsk.analysis.text.TextSpan
import kotlin.reflect.KClass

internal class DiagnosticBag {
    private val _diagnostics = mutableListOf<Diagnostic>()
    fun toList(): List<Diagnostic> = _diagnostics

    private fun report(span: TextSpan, message: String) {
        _diagnostics.add(Diagnostic(span, message))
    }

    fun reportBadCharacter(position: Int, char: Char) {
        report(TextSpan(position, 1), "Bad character in input: '$char'")
    }

    fun reportInvalidInt(span: TextSpan, text: String, type: KClass<out Any>) {
        report(span, "The number '$text' doesn't fit in '$type'")
    }

    fun reportUnexpectedToken(span: TextSpan, expected: SyntaxKind, actual: SyntaxKind) {
        report(span, "Expected next token to be $expected, got $actual instead")
    }

    fun reportUndefinedBinaryOperator(
        span: TextSpan,
        op: String,
        leftType: KClass<out Any>,
        rightType: KClass<out Any>,
    ) {
        report(span, "The binary operator '$op' isn't defined for '$leftType' and '$rightType'")
    }

    fun reportUndefinedUnaryOperator(span: TextSpan, op: String, operandType: KClass<out Any>) {
        report(span, "The unary operator '$op' isn't defined for '$operandType'")
    }

    fun addAll(diagnostics: Collection<Diagnostic>) {
        _diagnostics.addAll(diagnostics)
    }

    fun reportUndefinedName(span: TextSpan, name: String) {
        report(span, "Undefined name '$name'")
    }
}