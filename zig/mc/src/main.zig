const std = @import("std");
const builtin = @import("builtin");
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const Compilation = @import("minsk").code_analysis.Compilation;
const Object = @import("minsk_runtime").Object;
const VariableSymbol = @import("minsk").code_analysis.VariableSymbol;
const tty_ext = @import("tty_ext");
const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("linenoise.h");
});

fn pickAllocator(normal_alloc: std.mem.Allocator, debug_alloc: std.mem.Allocator) std.mem.Allocator {
    return switch (builtin.mode) {
        .Debug => debug_alloc,
        else => normal_alloc,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 10 }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var line_arena = std.heap.ArenaAllocator.init(allocator);
    defer line_arena.deinit();
    const line_alloc = pickAllocator(line_arena.allocator(), allocator);
    var stderr_buf = std.io.bufferedWriter(std.io.getStdErr().writer());
    const stderr = stderr_buf.writer();
    try tty_ext.enableAnsiEscapes(std.io.getStdErr());
    const tty = std.io.tty.detectConfig(std.io.getStdErr());
    var text_builder = std.ArrayList(u8).init(line_alloc);
    defer text_builder.deinit();
    var previous: ?*Compilation = null;
    defer if (previous) |p| p.deinit(.with_parents);

    const histfile_name = ".minsk-history";
    _ = c.linenoiseHistoryLoad(histfile_name);
    defer {
        _ = c.linenoiseHistorySave(histfile_name);
        c.linenoiseHistoryFree();
    }

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

    mainLoop: while (true) {
        const raw_line = blk: {
            const begin_prompt = "» ";
            const continue_prompt = "· ";
            const buf_len =
                "\x1b[0;32m".len +
                comptime @max(begin_prompt.len, continue_prompt.len) +
                "\x1b[0m".len +
                1;
            var prompt_buf: [buf_len]u8 = undefined;

            const prompt = std.fmt.bufPrintZ(&prompt_buf, "{s}{s}{s}", .{
                tty_ext.colorString(tty, .green),
                if (text_builder.items.len == 0) begin_prompt else continue_prompt,
                tty_ext.colorString(tty, .reset),
            }) catch unreachable;
            break :blk c.linenoise(prompt.ptr) orelse break :mainLoop;
        };
        _ = c.linenoiseHistoryAdd(raw_line);
        defer c.free(raw_line);
        const input_line = raw_line[0..std.mem.indexOfSentinel(u8, 0, raw_line)];
        defer stderr_buf.flush() catch unreachable;

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
            } else if (std.mem.eql(u8, input_line, "#reset")) {
                if (previous) |p| {
                    p.deinit(.with_parents);
                    previous = null;
                }
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

        if (show_tree) {
            try tree.root.base.prettyPrint(
                parser_alloc,
                "",
                true,
                stderr,
                .colors,
                tty,
            );
            stderr_buf.flush() catch unreachable;
        }

        const compilation = if (previous) |p|
            try p.continueWith(tree)
        else
            try Compilation.init(parser_alloc, tree);
        const result = try compilation.evaluate(&variables);
        defer result.deinit(parser_alloc);

        switch (result) {
            .failure => |diagnostics| {
                const text = tree.source;
                for (diagnostics) |d| {
                    const line_idx = text.getLineIndex(d.span.start) orelse unreachable;
                    const line_num = line_idx + 1;
                    const line = text.lines[line_idx];
                    const col_num = d.span.start - line.start + 1;

                    tty_ext.setColor(tty, stderr, .red) catch unreachable;
                    stderr.print("({d}, {d}): {s}\n", .{ line_num, col_num, d.message }) catch unreachable;
                    tty_ext.setColor(tty, stderr, .reset) catch unreachable;

                    const prefix = text.text[line.start..d.span.start];
                    const err = text.text[d.span.start..d.span.end()];
                    const suffix = text.text[d.span.end()..line.end()];
                    stderr.print("    {s}", .{prefix}) catch unreachable;
                    tty_ext.setColor(tty, stderr, .red) catch unreachable;
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
