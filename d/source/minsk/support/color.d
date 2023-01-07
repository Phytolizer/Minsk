module minsk.support.color;

import std.conv : to;
import std.stdio : write;
import std.traits : CommonType, isExpressions;

private enum ESC = "\x1b[";

string esc(int value) {
    return ESC ~ value.to!string ~ "m";
}

enum Style : int {
    reset = 0,
    bold,
    faint,
    italic,
    underline,
    slowBlink,
    rapidBlink,
    reverse,
    conceal,
    crossed,
}

enum Foreground : int {
    black = 30,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    reset,
}

alias Fg = Foreground;

enum Background : int {
    black = 40,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    reset,
}

alias Bg = Background;

enum ForegroundBright : int {
    black = 90,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    reset,
}

alias FgBright = ForegroundBright;

enum BackgroundBright : int {
    black = 100,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    reset,
}

alias BgBright = BackgroundBright;

void color(int[] colors...) {
    foreach (c; colors) {
        c.esc.write;
    }
}
