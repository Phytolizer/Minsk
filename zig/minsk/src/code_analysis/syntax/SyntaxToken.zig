const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const Object = @import("minsk_runtime").Object;
const SyntaxNode = @import("SyntaxNode.zig");

base: SyntaxNode,
kind: SyntaxKind,
position: usize,
text: []const u8,
value: ?Object,

const Self = @This();

pub fn init(
    kind: SyntaxKind,
    position: usize,
    text: []const u8,
    value: ?Object,
) Self {
    return .{
        .base = .{
            .kind = kind,
            .deinit_fn = &deinit,
            .children_fn = &children,
        },
        .kind = kind,
        .position = position,
        .text = text,
        .value = value,
    };
}

fn deinit(_: *const SyntaxNode, _: std.mem.Allocator) void {}

fn children(_: *const SyntaxNode, _: std.mem.Allocator) std.mem.Allocator.Error![]const *const SyntaxNode {
    return &.{};
}

pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("{s}: '{s}'", .{ self.kind.displayName(), self.text });
    if (self.value) |value| {
        try writer.print(" {}", .{value});
    }
}
