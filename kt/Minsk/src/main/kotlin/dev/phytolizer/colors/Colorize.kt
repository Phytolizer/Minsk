package dev.phytolizer.colors

object Colorize {
    private const val ESC: String = "\u001b["
    const val RESET: String = "${ESC}0m"

    fun colorCode(color: AnsiColor, style: ColorStyle): String = when (style) {
        ColorStyle.Regular -> "${ESC}0;3${color.code}m"
        ColorStyle.Bold -> "${ESC}1;3${color.code}m"
        ColorStyle.Bright -> "${ESC}0;9${color.code}m"
        ColorStyle.BoldBright -> "${ESC}1;9${color.code}m"
        ColorStyle.Underlined -> "${ESC}4;3${color.code}m"
        ColorStyle.Background -> "${ESC}4${color.code}m"
        ColorStyle.BrightBackground -> "${ESC}0;10${color.code}m"
    }

    fun clearScreen() {
        print("${ESC}2J")
    }

    fun colorCode256(index: Int): String = "${ESC}38;5;${index}m"
}
