package dev.phytolizer.minsk.analysis.text

import io.kotest.core.spec.style.FunSpec
import io.kotest.data.forAll
import io.kotest.data.row
import io.kotest.matchers.shouldBe

@Suppress("unused")
class SourceTextTests : FunSpec({
    test("SourceText includes last line") {
        forAll(
            row(".", 1),
            row(".\r\n", 2),
            row(".\r\n\r\n", 3),
        ) { text, expectedLineCount ->
            val sourceText = SourceText.from(text)
            sourceText.lines.size shouldBe expectedLineCount
        }
    }
})
