const std = @import("std");

start: usize,
length: usize,

const Self = @This();

pub fn fromBounds(start: usize, e: usize) Self {
    return .{
        .start = start,
        .length = e - start,
    };
}

pub fn end(self: Self) usize {
    return self.start + self.length;
}

pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("{d}..{d}", .{ self.start, self.end() });
}
