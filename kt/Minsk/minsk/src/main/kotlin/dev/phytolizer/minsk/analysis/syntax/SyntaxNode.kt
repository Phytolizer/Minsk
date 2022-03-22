package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.TextSpan

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
        prettyPrint(this, "", true)
    }

    private fun prettyPrint(node: SyntaxNode, indent: String, isLast: Boolean) {
        print(indent)
        val marker = if (isLast) {
            "└── "
        } else {
            "├── "
        }
        print(marker)
        if (node is SyntaxToken) {
            print(node)
        } else {
            print(node.kind)
        }
        println()
        val newIndent = indent + if (isLast) {
            "    "
        } else {
            "│   "
        }
        val lastChild = node.children.lastOrNull()
        for (child in node.children) {
            prettyPrint(child, newIndent, child == lastChild)
        }
    }
}
