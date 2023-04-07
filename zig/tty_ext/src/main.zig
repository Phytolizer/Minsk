const std = @import("std");
const builtin = @import("builtin");

pub fn clearScreen(tty: std.debug.TTY.Config, writer: anytype) !void {
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
                @intCast(
                    std.os.windows.DWORD,
                    screen_info.dwSize.X * screen_info.dwSize.Y,
                ),
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
    gray,
    cyan,
    blue,
    green,
    magenta,
    reset,
};

pub fn setColor(conf: std.debug.TTY.Config, out_stream: anytype, color: Color) !void {
    nosuspend switch (conf) {
        .no_color => return,
        .escape_codes => {
            const color_string = switch (color) {
                .dim_red => "\x1b[31;2m",
                .gray => "\x1b[2m",
                .cyan => "\x1b[0;36m",
                .blue => "\x1b[0;34m",
                .green => "\x1b[0;32m",
                .magenta => "\x1b[0;35m",
                .reset => "\x1b[0m",
            };
            try out_stream.writeAll(color_string);
        },
        .windows_api => |ctx| if (builtin.os.tag == .windows) {
            const windows = std.os.windows;
            const attributes = switch (color) {
                .dim_red => windows.FOREGROUND_RED,
                .gray => windows.FOREGROUND_INTENSITY,
                .cyan => windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE,
                .blue => windows.FOREGROUND_BLUE,
                .green => windows.FOREGROUND_GREEN,
                .magenta => windows.FOREGROUND_RED | windows.FOREGROUND_BLUE,
                .reset => ctx.reset_attributes,
            };
            try windows.SetConsoleTextAttribute(ctx.handle, attributes);
        } else {
            unreachable;
        },
    };
}

pub fn resetColor(tty: std.debug.TTY.Config, buf: anytype) void {
    if (builtin.os.tag == .windows) buf.flush() catch unreachable;
    setColor(tty, buf.writer(), .reset) catch unreachable;
}
