package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.analysis.text.TextSpan
import java.io.PrintStream

abstract class SyntaxNode {
    abstract val kind: SyntaxKind
    abstract val children: List<SyntaxNode>

    open val span: TextSpan
        get() {
            val first = children.first().span
            val last = children.last().span
            return TextSpan.fromBounds(first.start, last.end)
        }

    fun prettyPrint() {
        prettyPrint(this, System.out, "", true)
    }

    fun writeTo(writer: PrintStream) {
        prettyPrint(this, writer, "", true)
    }

    private fun prettyPrint(node: SyntaxNode, writer: PrintStream, indent: String, isLast: Boolean) {
        val isToConsole = writer == System.out
        val marker = if (isLast) {
            "└── "
        } else {
            "├── "
        }
        if (isToConsole) {
            writer.print(Colorize.colorCode256(243))
        }
        writer.print(indent)
        if (isToConsole) {
            writer.print(marker)
        }

        if (isToConsole) {
            writer.print(Colorize.colorCode(if (node is SyntaxToken) {
                AnsiColor.Blue
            } else {
                AnsiColor.Cyan
            }, ColorStyle.Regular))
        }
        writer.print(node.kind)
        if (isToConsole) {
            writer.print(Colorize.RESET)
        }
        if (node is SyntaxToken && node.value != null) {
            writer.print(" ${node.value}")
        }

        writer.println()
        val newIndent = indent + if (isLast) {
            "    "
        } else {
            "│   "
        }
        val lastChild = node.children.lastOrNull()
        for (child in node.children) {
            prettyPrint(child, writer, newIndent, child == lastChild)
        }
    }
}
