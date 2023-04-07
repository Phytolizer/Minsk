const std = @import("std");
const builtin = @import("builtin");
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const Compilation = @import("minsk").code_analysis.Compilation;
const Object = @import("minsk_runtime").Object;
const VariableSymbol = @import("minsk").code_analysis.VariableSymbol;
const tty_ext = @import("tty_ext");

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
    var variables = VariableSymbol.Map.init(allocator);
    defer {
        for (variables.keys()) |vs| {
            allocator.free(vs.name);
        }
        variables.deinit();
    }

    if (builtin.os.tag == .windows) {
        // Ensure we are using UTF-8
        const utf8_codepage = 65001;
        const windows = std.os.windows;
        const kernel32 = windows.kernel32;
        if (kernel32.SetConsoleOutputCP(utf8_codepage) == 0) {
            switch (kernel32.GetLastError()) {
                else => |err| return windows.unexpectedError(err),
            }
        }
    }

    while (true) {
        stderr.writeAll("> ") catch unreachable;
        stderr_buf.flush() catch unreachable;
        const input_line = blk: {
            var line = try readUntilDelimiterOrEofArrayList(
                stdin,
                &line_buf,
                '\n',
                std.math.maxInt(usize),
            ) orelse {
                stderr.writeByte('\n') catch unreachable;
                stderr_buf.flush() catch unreachable;
                break;
            };
            line = std.mem.trimRight(u8, line, "\r");
            break :blk line;
        };

        if (std.mem.eql(u8, input_line, "#showTree")) {
            show_tree = !show_tree;
            stderr.print("{s}\n", .{
                if (show_tree)
                    "Showing parse trees."
                else
                    "Not showing parse trees.",
            }) catch unreachable;
            continue;
        } else if (std.mem.eql(u8, input_line, "#cls")) {
            try tty_ext.clearScreen(tty, stderr);
            continue;
        }

        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        defer parser_arena.deinit();
        const parser_alloc = pickAllocator(parser_arena.allocator(), allocator);

        const tree = try SyntaxTree.parse(parser_alloc, input_line);
        var compilation = Compilation.init(parser_alloc, tree);
        defer compilation.deinit();
        const result = try compilation.evaluate(&variables);
        defer result.deinit(parser_alloc);

        if (show_tree) {
            try compilation.syntax_tree.root.base.prettyPrint(
                parser_alloc,
                "",
                true,
                stderr,
                .colors,
                tty,
                &stderr_buf,
            );
        }

        switch (result) {
            .failure => |diagnostics| {
                const text = tree.source;
                for (diagnostics) |d| {
                    tty_ext.setColor(tty, stderr, .dim_red) catch unreachable;
                    stderr.print("{s}\n", .{d}) catch unreachable;
                    tty_ext.resetColor(tty, &stderr_buf);

                    const line_idx = text.getLineIndex(d.span.start) orelse unreachable;
                    const line_num = line_idx + 1;
                    const text_line = text.lines[line_idx];
                    const line = text.text[text_line.start..text_line.end()];

                    const prefix = line[0..d.span.start];
                    const err = line[d.span.start..d.span.end()];
                    const suffix = line[d.span.end()..];
                    const col_num = d.span.start + 1;
                    stderr.print("({d}, {d}):    {s}", .{ line_num, col_num, prefix }) catch unreachable;
                    tty_ext.setColor(tty, stderr, .dim_red) catch unreachable;
                    stderr.print("{s}", .{err}) catch unreachable;
                    tty_ext.resetColor(tty, &stderr_buf);
                    stderr.print("{s}\n", .{suffix}) catch unreachable;
                }
            },
            .success => |value| {
                stderr.print("{d}\n", .{value}) catch unreachable;
            },
        }

        for (variables.keys()) |*vs| {
            if (!vs.duped) {
                vs.name = try allocator.dupe(u8, vs.name);
                vs.duped = true;
            }
        }
    }
}
