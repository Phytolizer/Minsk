const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const Object = @import("minsk_runtime").Object;
const SyntaxNode = @import("SyntaxNode.zig");
const TextSpan = @import("../text/TextSpan.zig");

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
            .span_fn = &span_override,
        },
        .kind = kind,
        .position = position,
        .text = text,
        .value = value,
    };
}

fn deinit(_: *const SyntaxNode, _: std.mem.Allocator) void {}

fn children(_: *const SyntaxNode, _: std.mem.Allocator) std.mem.Allocator.Error![]*const SyntaxNode {
    return &.{};
}

fn span_override(node: *const SyntaxNode, _: std.mem.Allocator) !TextSpan {
    return SyntaxNode.downcast(node, Self).span();
}

pub fn span(self: Self) TextSpan {
    return .{
        .start = self.position,
        .length = self.text.len,
    };
}
