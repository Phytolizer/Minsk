const std = @import("std");
const builtin = @import("builtin");

pub fn clearScreen(tty: std.io.tty.Config, writer: anytype) !void {
    nosuspend switch (tty) {
        .no_color => return,
        .escape_codes => {
            const clear_string = "\x1b[2J\x1b[H";
            try writer.writeAll(clear_string);
        },
        .windows_api => |wapi| if (builtin.os.tag == .windows) {
            var screen_info: std.os.windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
            const kernel32 = std.os.windows.kernel32;
            if (kernel32.GetConsoleScreenBufferInfo(wapi.handle, &screen_info) == 0) {
                switch (kernel32.GetLastError()) {
                    else => |err| return std.os.windows.unexpectedError(err),
                }
            }
            var num_chars: std.os.windows.DWORD = undefined;
            if (kernel32.FillConsoleOutputCharacterA(
                wapi.handle,
                ' ',
                @intCast(screen_info.dwSize.X * screen_info.dwSize.Y),
                std.os.windows.COORD{ .X = 0, .Y = 0 },
                &num_chars,
            ) == 0) {
                switch (kernel32.GetLastError()) {
                    else => |err| return std.os.windows.unexpectedError(err),
                }
            }
            if (kernel32.SetConsoleCursorPosition(
                wapi.handle,
                std.os.windows.COORD{ .X = 0, .Y = 0 },
            ) == 0) {
                switch (kernel32.GetLastError()) {
                    else => |err| return std.os.windows.unexpectedError(err),
                }
            }
        } else unreachable,
    };
}

pub const Color = enum {
    dim_red,
    dim_yellow,
    red,
    gray,
    cyan,
    blue,
    green,
    magenta,
    yellow,
    reset,
};

pub fn colorString(conf: std.io.tty.Config, color: Color) []const u8 {
    return switch (conf) {
        .no_color => "",
        else => switch (color) {
            .dim_red => "\x1b[31;2m",
            .dim_yellow => "\x1b[33;2m",
            .red => "\x1b[0;31m",
            .gray => "\x1b[2m",
            .cyan => "\x1b[0;36m",
            .blue => "\x1b[0;34m",
            .green => "\x1b[0;32m",
            .magenta => "\x1b[0;35m",
            .yellow => "\x1b[0;33m",
            .reset => "\x1b[0m",
        },
    };
}

pub fn setColor(conf: std.io.tty.Config, out_stream: anytype, color: Color) !void {
    nosuspend switch (conf) {
        .no_color => return,
        else => {
            try out_stream.writeAll(colorString(conf, color));
        },
    };
}

pub fn maybeSetColor(conf: ?std.io.tty.Config, out_stream: anytype, color: Color) !void {
    if (conf) |c|
        try setColor(c, out_stream, color);
}

extern "kernel32" fn SetConsoleMode(
    in_hConsoleHandle: std.os.windows.HANDLE,
    in_dwMode: std.os.windows.DWORD,
) callconv(std.os.windows.WINAPI) std.os.windows.BOOL;

pub fn enableAnsiEscapes(f: std.fs.File) !void {
    if (builtin.os.tag == .windows) {
        const windows = std.os.windows;
        var mode: windows.DWORD = undefined;
        if (windows.kernel32.GetConsoleMode(f.handle, &mode) == 0)
            switch (windows.kernel32.GetLastError()) {
                else => |err| return std.os.windows.unexpectedError(err),
            };

        const ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004;
        mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
        if (SetConsoleMode(f.handle, mode) == 0)
            switch (windows.kernel32.GetLastError()) {
                else => |err| return std.os.windows.unexpectedError(err),
            };
    }
}
