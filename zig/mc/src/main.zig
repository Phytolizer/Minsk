const std = @import("std");
const Lexer = @import("minsk").code_analysis.syntax.Lexer;

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
    var line_alloc = line_arena.allocator();
    var line_buf = std.ArrayList(u8).init(line_alloc);
    var stderr_buf = std.io.bufferedWriter(std.io.getStdErr().writer());
    const stderr = stderr_buf.writer();
    const stdin = std.io.getStdIn().reader();

    var lexer = try Lexer.init(line_alloc, "123 hi");
    const token = try lexer.nextToken();
    std.debug.print("{}\n", .{token.?});

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

        if (std.mem.eql(u8, line, "1 + 2 * 3")) {
            stderr.writeAll("7\n") catch unreachable;
        } else {
            stderr.writeAll("ERROR: Invalid expression!\n") catch unreachable;
        }
    }
}
