const std = @import("std");
const builtin = @import("builtin");
const Parser = @import("minsk").code_analysis.syntax.Parser;

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

        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        defer parser_arena.deinit();
        const parser_alloc = pickAllocator(parser_arena.allocator(), allocator);

        var parser = try Parser.init(parser_alloc, line);
        defer parser.deinit();
        const expression = try parser.parse();
        defer expression.deinit(parser_alloc);

        if (parser.diagnostics.items.len > 0) {
            tty.setColor(stderr, .Red) catch unreachable;
            tty.setColor(stderr, .Dim) catch unreachable;
            defer tty.setColor(stderr, .Reset) catch unreachable;

            for (parser.diagnostics.items) |d| {
                stderr.print("{s}\n", .{d}) catch unreachable;
            }
        } else {
            tty.setColor(stderr, .Dim) catch unreachable;
            defer tty.setColor(stderr, .Reset) catch unreachable;
            try expression.base.prettyPrint(parser_alloc, "", true, stderr);
        }
    }
}
