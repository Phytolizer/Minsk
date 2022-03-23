package dev.phytolizer.minsk

import io.kotest.core.config.AbstractProjectConfig
import io.kotest.core.names.DuplicateTestNameMode

class TestConfig : AbstractProjectConfig() {
    override val duplicateTestNameMode: DuplicateTestNameMode
        get() = DuplicateTestNameMode.Silent
}
