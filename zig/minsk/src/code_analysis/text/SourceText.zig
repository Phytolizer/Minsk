const std = @import("std");
const TextSpan = @import("TextSpan.zig");
const glyph = @import("ziglyph");

allocator: std.mem.Allocator,
text: []const u8,
lines: []const Line,

const SourceText = @This();

pub fn from(allocator: std.mem.Allocator, text: []const u8) !*const SourceText {
    const self = try allocator.create(SourceText);
    self.allocator = allocator;
    self.text = text;
    self.lines = try self.parseLines(allocator, text);
    return self;
}

pub fn deinit(self: *const SourceText) void {
    self.allocator.free(self.lines);
    self.allocator.destroy(self);
}

fn parseLines(self: *SourceText, allocator: std.mem.Allocator, text: []const u8) ![]const Line {
    var result = std.ArrayList(Line).init(allocator);
    defer result.deinit();

    const view = try std.unicode.Utf8View.init(text);
    var it = view.iterator();
    var line_start = it.i;
    while (true) {
        const line_break_width = getLineBreakWidth(&it) orelse break;
        if (line_break_width > 0) {
            try self.addLine(&result, it.i, line_start, line_break_width);
            line_start = it.i;
        }
    }
    if (it.i > line_start) {
        try self.addLine(&result, it.i, line_start, 0);
    }
    return try result.toOwnedSlice();
}

fn addLine(self: *SourceText, lines: *std.ArrayList(Line), pos: usize, line_start: usize, line_break_width: usize) !void {
    try lines.append(.{
        .source = self,
        .start = line_start,
        .length = pos - line_break_width - line_start,
        .length_including_line_break = pos - line_start,
    });
}

fn getLineBreakWidth(inout_pos: *std.unicode.Utf8Iterator) ?usize {
    const cp = inout_pos.nextCodepoint() orelse return null;
    const look = inout_pos.nextCodepoint();
    return if (cp == '\r' and look != null and look.? == '\n')
        2
    else if (cp == '\r' or cp == '\n')
        1
    else
        0;
}

pub fn getLineIndex(self: *const SourceText, position: usize) ?usize {
    const compare = struct {
        fn cmp(_: void, key: usize, mid: Line) std.math.Order {
            return if (mid.start > key)
                .gt
            else if (mid.end() < key)
                .lt
            else
                .eq;
        }
    }.cmp;
    return std.sort.binarySearch(Line, position, self.lines, {}, compare);
}

pub const Line = struct {
    source: *const SourceText,
    start: usize,
    length: usize,
    length_including_line_break: usize,

    pub fn end(self: @This()) usize {
        return self.start + self.length;
    }

    pub fn endIncludingLineBreak(self: @This()) usize {
        return self.start + self.length_including_line_break;
    }

    pub fn span(self: @This()) TextSpan {
        return .{ .start = self.start, .length = self.length };
    }

    pub fn spanIncludingLineBreak(self: @This()) TextSpan {
        return .{
            .start = self.start,
            .length = self.length_including_line_break,
        };
    }
};
