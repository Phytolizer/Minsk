package dev.phytolizer.colors

sealed class ColorStyle {
    object Regular : ColorStyle()
    object Bold : ColorStyle()
    object Bright : ColorStyle()
    object BoldBright : ColorStyle()
    object Underlined : ColorStyle()
    object Background : ColorStyle()
    object BrightBackground : ColorStyle()

    override fun toString(): String = javaClass.simpleName
}
