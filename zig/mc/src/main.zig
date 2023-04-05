const std = @import("std");
const builtin = @import("builtin");
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const Compilation = @import("minsk").code_analysis.Compilation;

fn readUntilDelimiterOrEofArrayList(
    writer: anytype,
    array_list: *std.ArrayList(u8),
    delimiter: u8,
    max_size: usize,
) !?[]const u8 {
    writer.readUntilDelimiterArrayList(
        array_list,
        delimiter,
        max_size,
    ) catch |e| switch (e) {
        error.EndOfStream => return if (array_list.items.len > 0)
            array_list.items
        else
            null,
        else => return e,
    };
    return array_list.items;
}

fn pickAllocator(normal_alloc: std.mem.Allocator, debug_alloc: std.mem.Allocator) std.mem.Allocator {
    return switch (builtin.mode) {
        .Debug => debug_alloc,
        else => normal_alloc,
    };
}

fn clearScreen(tty: std.debug.TTY.Config, writer: anytype) !void {
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

const Color = enum {
    dim_red,
    gray,
    reset,
};

pub fn setColor(conf: std.debug.TTY.Config, out_stream: anytype, color: Color) !void {
    nosuspend switch (conf) {
        .no_color => return,
        .escape_codes => {
            const color_string = switch (color) {
                .dim_red => "\x1b[31;2m",
                .gray => "\x1b[2m",
                .reset => "\x1b[0m",
            };
            try out_stream.writeAll(color_string);
        },
        .windows_api => |ctx| if (builtin.os.tag == .windows) {
            const windows = std.os.windows;
            const attributes = switch (color) {
                .dim_red => windows.FOREGROUND_RED,
                .gray => windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE,
                .Reset => ctx.reset_attributes,
            };
            try windows.SetConsoleTextAttribute(ctx.handle, attributes);
        } else {
            unreachable;
        },
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var line_arena = std.heap.ArenaAllocator.init(allocator);
    defer line_arena.deinit();
    const line_alloc = pickAllocator(line_arena.allocator(), allocator);
    var line_buf = std.ArrayList(u8).init(line_alloc);
    defer line_buf.deinit();
    var stderr_buf = std.io.bufferedWriter(std.io.getStdErr().writer());
    const stderr = stderr_buf.writer();
    const tty = std.debug.detectTTYConfig(std.io.getStdErr());
    const stdin = std.io.getStdIn().reader();

    var show_tree = false;

    while (true) {
        stderr.writeAll("> ") catch unreachable;
        stderr_buf.flush() catch unreachable;
        const line = try readUntilDelimiterOrEofArrayList(
            stdin,
            &line_buf,
            '\n',
            std.math.maxInt(usize),
        ) orelse {
            stderr.writeByte('\n') catch unreachable;
            stderr_buf.flush() catch unreachable;
            break;
        };

        if (std.mem.eql(u8, line, "#showTree")) {
            show_tree = !show_tree;
            stderr.print("{s}\n", .{
                if (show_tree)
                    "Showing parse trees."
                else
                    "Not showing parse trees.",
            }) catch unreachable;
            continue;
        } else if (std.mem.eql(u8, line, "#cls")) {
            try clearScreen(tty, stderr);
            continue;
        }

        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        defer parser_arena.deinit();
        const parser_alloc = pickAllocator(parser_arena.allocator(), allocator);

        const tree = try SyntaxTree.parse(parser_alloc, line);
        var compilation = Compilation.init(parser_alloc, tree);
        defer compilation.deinit();
        const result = try compilation.evaluate();
        defer result.deinit(parser_alloc);

        if (show_tree) {
            setColor(tty, stderr, .gray) catch unreachable;
            defer setColor(tty, stderr, .reset) catch unreachable;
            try compilation.syntax_tree.root.base.prettyPrint(parser_alloc, "", true, stderr);
        }

        switch (result) {
            .failure => |diagnostics| {
                setColor(tty, stderr, .dim_red) catch unreachable;
                defer setColor(tty, stderr, .reset) catch unreachable;

                for (diagnostics) |d| {
                    stderr.print("{s}\n", .{d}) catch unreachable;
                }
            },
            .success => |value| {
                stderr.print("{d}\n", .{value}) catch unreachable;
            },
        }
    }
}
