const std = @import("std");
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var line_arena = std.heap.ArenaAllocator.init(allocator);
    defer line_arena.deinit();
    const line_alloc = line_arena.allocator();
    var line_buf = std.ArrayList(u8).init(line_alloc);
    var stderr_buf = std.io.bufferedWriter(std.io.getStdErr().writer());
    const stderr = stderr_buf.writer();
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
        const parser_alloc = parser_arena.allocator();

        var parser = try Parser.init(parser_alloc, line);
        const expression = try parser.parse();
        try expression.base.prettyPrint(parser_alloc, "", true, std.io.getStdErr().writer());
    }
}
