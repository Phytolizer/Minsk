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
    try tty_ext.enableAnsiEscapes(std.io.getStdErr());
    const tty = std.debug.detectTTYConfig(std.io.getStdErr());
    const stdin = std.io.getStdIn().reader();
    var text_builder = std.ArrayList(u8).init(line_alloc);
    defer text_builder.deinit();
    var previous: ?*Compilation = null;
    defer if (previous) |p| p.deinit(.with_parents);

    var show_tree = false;
    var variables = VariableSymbol.Map.init(allocator);
    defer {
        for (variables.keys()) |vs| {
            vs.deinit(allocator);
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
        tty_ext.setColor(tty, stderr, .green) catch unreachable;
        stderr.writeAll(
            if (text_builder.items.len == 0)
                "» "
            else
                "· ",
        ) catch unreachable;
        tty_ext.setColor(tty, stderr, .reset) catch unreachable;
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

        const is_blank = input_line.len == 0;

        if (text_builder.items.len == 0) {
            if (is_blank) continue;
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
        }

        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        defer parser_arena.deinit();
        const parser_alloc = pickAllocator(parser_arena.allocator(), allocator);

        try text_builder.appendSlice(input_line);
        try text_builder.append('\n');
        const input_text = text_builder.items;
        const tree = try SyntaxTree.parse(parser_alloc, input_text);
        if (tree.diagnostics.?.diagnostics.items.len > 0 and !is_blank) {
            tree.deinit();
            continue;
        }
        const compilation = if (previous) |p|
            try p.continueWith(tree)
        else
            try Compilation.init(parser_alloc, tree);
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
            );
        }

        switch (result) {
            .failure => |diagnostics| {
                const text = tree.source;
                for (diagnostics) |d| {
                    const line_idx = text.getLineIndex(d.span.start) orelse unreachable;
                    const line_num = line_idx + 1;
                    const line = text.lines[line_idx];
                    const col_num = d.span.start - line.start + 1;

                    tty_ext.setColor(tty, stderr, .dim_red) catch unreachable;
                    stderr.print("({d}, {d}): {s}\n", .{ line_num, col_num, d }) catch unreachable;
                    tty_ext.setColor(tty, stderr, .reset) catch unreachable;

                    const prefix = text.text[line.start..d.span.start];
                    const err = text.text[d.span.start..d.span.end()];
                    const suffix = text.text[d.span.end()..line.end()];
                    stderr.print("    {s}", .{prefix}) catch unreachable;
                    tty_ext.setColor(tty, stderr, .dim_red) catch unreachable;
                    stderr.print("{s}", .{err}) catch unreachable;
                    tty_ext.setColor(tty, stderr, .reset) catch unreachable;
                    stderr.print("{s}\n", .{suffix}) catch unreachable;
                }
                compilation.deinit(.without_parents);
            },
            .success => |value| {
                try tty_ext.setColor(tty, stderr, .magenta);
                defer tty_ext.setColor(tty, stderr, .reset) catch unreachable;
                stderr.print("{d}\n", .{value}) catch unreachable;
                previous = compilation;
            },
        }

        for (variables.keys()) |*vs| {
            if (!vs.duped) {
                vs.name = try allocator.dupe(u8, vs.name);
                vs.duped = true;
            }
        }

        text_builder.clearRetainingCapacity();
    }
}
