package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.TextSpan
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
        writer.print(indent)
        val marker = if (isLast) {
            "└── "
        } else {
            "├── "
        }
        writer.print(marker)
        if (node is SyntaxToken) {
            writer.print(node)
        } else {
            writer.print(node.kind)
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
