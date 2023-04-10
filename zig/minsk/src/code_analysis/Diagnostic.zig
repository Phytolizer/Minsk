const std = @import("std");
const TextSpan = @import("text/TextSpan.zig");

span: TextSpan,
message: []const u8,

const Self = @This();

pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
    allocator.free(self.message);
}
