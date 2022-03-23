package dev.phytolizer.minsk.analysis.syntax

import io.kotest.matchers.iterator.shouldBeEmpty
import io.kotest.matchers.iterator.shouldHaveNext
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeTypeOf
import io.kotest.matchers.types.shouldNotBeTypeOf

class AssertingEnumerator(root: SyntaxNode) {
    private val _enumerator: Iterator<SyntaxNode>

    companion object {
        private fun flatten(root: SyntaxNode): List<SyntaxNode> {
            val result = mutableListOf<SyntaxNode>()
            val stack = mutableListOf(root)

            while (stack.size > 0) {
                val n = stack.removeLast()
                result.add(n)

                for (child in n.children.asReversed()) {
                    stack.add(child)
                }
            }

            return result
        }
    }

    init {
        _enumerator = flatten(root).iterator()
    }

    fun assertNode(kind: SyntaxKind) {
        _enumerator.shouldHaveNext()
        val current = _enumerator.next()
        current.kind shouldBe kind
        current.shouldNotBeTypeOf<SyntaxToken>()
    }

    fun assertToken(kind: SyntaxKind, text: String) {
        _enumerator.shouldHaveNext()
        val current = _enumerator.next()
        current.kind shouldBe kind
        val token = current.shouldBeTypeOf<SyntaxToken>()
        token.text shouldBe text
    }

    fun assertAtEnd() {
        _enumerator.shouldBeEmpty()
    }
}
