const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const SyntaxToken = @import("SyntaxToken.zig");
const DowncastedPointer = @import("ptr_downcast.zig").DowncastedPointer;

pub const DeinitFn = *const fn (self: *const Self, allocator: std.mem.Allocator) void;
pub const ChildrenFn = *const fn (self: *const Self, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const *const Self;

kind: SyntaxKind,
deinit_fn: DeinitFn,
children_fn: ChildrenFn,

const Self = @This();

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.deinit_fn(self, allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]const *const Self {
    return try self.children_fn(self, allocator);
}

pub fn prettyPrint(
    self: *const Self,
    allocator: std.mem.Allocator,
    indent: []const u8,
    is_last: bool,
    writer: anytype,
) !void {
    try writer.print("{s}{s}{s}", .{
        indent,
        if (is_last)
            "└───"
        else
            "├───",
        self.kind.displayName(),
    });
    if (std.mem.endsWith(u8, self.kind.displayName(), "Token")) {
        const token = self.downcast(SyntaxToken);
        if (token.value) |value| {
            try writer.print(" {}", .{value});
        }
    }
    try writer.writeAll("\n");
    const new_indent = try std.mem.concat(allocator, u8, &.{
        indent, if (is_last)
            "    "
        else
            "│   ",
    });
    defer allocator.free(new_indent);

    const cs = try self.children(allocator);
    defer allocator.free(cs);
    for (cs, 0..) |c, i| {
        try c.prettyPrint(allocator, new_indent, i == cs.len - 1, writer);
    }
}
