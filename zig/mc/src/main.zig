const std = @import("std");
const builtin = @import("builtin");
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const Evaluator = @import("minsk").code_analysis.Evaluator;
const Binder = @import("minsk").code_analysis.binding.Binder;

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

        var tree = try SyntaxTree.parse(parser_alloc, line);
        defer tree.deinit();
        var binder = Binder.init(parser_alloc);
        defer binder.deinit();
        const bound_expression = try binder.bindExpression(tree.root);
        defer bound_expression.deinit(parser_alloc);

        const diagnostics = blk: {
            const slices = [_][][]const u8{
                tree.takeDiagnostics(),
                try binder.diagnostics.toOwnedSlice(),
            };
            defer for (slices) |s| {
                parser_alloc.free(s);
            };
            break :blk try std.mem.concat(parser_alloc, []const u8, &slices);
        };
        defer {
            for (diagnostics) |d| {
                parser_alloc.free(d);
            }
            parser_alloc.free(diagnostics);
        }

        if (show_tree) {
            tty.setColor(stderr, .Dim) catch unreachable;
            defer tty.setColor(stderr, .Reset) catch unreachable;
            try tree.root.base.prettyPrint(parser_alloc, "", true, stderr);
        }

        if (diagnostics.len > 0) {
            tty.setColor(stderr, .Red) catch unreachable;
            tty.setColor(stderr, .Dim) catch unreachable;
            defer tty.setColor(stderr, .Reset) catch unreachable;

            for (diagnostics) |d| {
                stderr.print("{s}\n", .{d}) catch unreachable;
            }
        } else {
            const evaluator = Evaluator.init(bound_expression);
            const result = evaluator.evaluate();
            stderr.print("{d}\n", .{result}) catch unreachable;
        }
    }
}
