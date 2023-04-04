const std = @import("std");

pub const Object = union(enum) {
    int: u64,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .int => |v| try writer.print("{d}", .{v}),
        }
    }
};
