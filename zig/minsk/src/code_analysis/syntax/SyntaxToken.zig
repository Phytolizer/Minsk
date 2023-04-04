const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const Object = @import("minsk_runtime").Object;

kind: SyntaxKind,
position: usize,
text: []const u8,
value: ?Object,

const Self = @This();

pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("{s}: '{s}'", .{ self.kind.displayName(), self.text });
    if (self.value) |value| {
        try writer.print(" {}", .{value});
    }
}
