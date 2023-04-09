const std = @import("std");
const TextSpan = @import("minsk").code_analysis.text.TextSpan;

text: []const u8,
spans: []const TextSpan,

const AnnotatedText = @This();

pub fn parse(comptime text: []const u8) AnnotatedText {
    comptime {
        const unindented = blk: {
            const lines = unindentLines(text);
            var result: []const u8 = "";
            for (lines) |line| {
                result = result ++ line ++ "\n";
            }
            break :blk result;
        };
        var result_text: []const u8 = "";
        var spans: []const TextSpan = &.{};
        var starts: []const usize = &.{};

        var position: usize = 0;
        for (unindented) |c| {
            switch (c) {
                '[' => starts = starts ++ &[_]usize{position},
                ']' => {
                    if (starts.len == 0) unreachable;

                    const start = starts[starts.len - 1];
                    starts = starts[0 .. starts.len - 1];
                    const end = position;
                    const span = TextSpan.fromBounds(start, end);
                    spans = spans ++ &[_]TextSpan{span};
                },
                else => {
                    position += 1;
                    result_text = result_text ++ &[_]u8{c};
                },
            }
        }

        if (starts.len > 0) unreachable;

        return AnnotatedText{
            .text = result_text,
            .spans = spans,
        };
    }
}

pub fn unindentLines(comptime text: []const u8) []const []const u8 {
    comptime {
        var lines: []const []const u8 = &.{};
        var pos = 0;
        while (pos < text.len) {
            // do not need to handle CR, these strings come from Zig source code
            const eol = std.mem.indexOfScalarPos(u8, text, pos, '\n') orelse text.len;
            const line = text[pos..eol];
            lines = lines ++ &[_][]const u8{line};
            pos = eol + 1;
        }

        comptime var min_indentation = std.math.maxInt(usize);
        for (lines) |line| {
            if (std.mem.trim(u8, line, " ").len == 0) {
                line.* = "";
                continue;
            }

            const indentation = line.len - std.mem.trimLeft(u8, line, " ").len;
            min_indentation = std.math.min(min_indentation, indentation);
        }

        const start = blk: {
            for (lines, 0..) |line, i| {
                if (line.len != 0)
                    break :blk i;
            }
            // no lines whatsoever
            return "";
        };

        const end = blk: {
            var it = std.mem.reverseIterator(lines);
            var i = lines.len;
            while (it.next()) |line| {
                if (line.len != 0)
                    break :blk i;
                i -= 1;
            }
            unreachable;
        };

        var result: []const []const u8 = &.{};
        for (lines[start..end]) |line| {
            if (line.len < min_indentation) {
                result = result ++ &[_][]const u8{line};
            } else {
                result = result ++ &[_][]const u8{line[min_indentation..]};
            }
        }
        return result;
    }
}
