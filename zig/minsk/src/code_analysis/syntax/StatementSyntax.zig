const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const TextSpan = @import("../text/TextSpan.zig");

base: SyntaxNode,

const Self = @This();

pub fn init(
    kind: SyntaxKind,
    deinit_fn: SyntaxNode.DeinitFn,
    children_fn: SyntaxNode.ChildrenFn,
    span_fn: ?SyntaxNode.SpanFn,
) Self {
    return .{
        .base = .{
            .kind = kind,
            .deinit_fn = deinit_fn,
            .children_fn = children_fn,
            .span_fn = span_fn,
        },
    };
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.base.deinit(allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]*const SyntaxNode {
    return try self.base.children(allocator);
}

pub fn span(self: *const Self, allocator: std.mem.Allocator) !TextSpan {
    return try self.base.span(allocator);
}

pub fn downcastNode(base: anytype, comptime T: type) DowncastedPointer(@TypeOf(base), T) {
    const self = @fieldParentPtr(Self, "base", base);
    return downcast(self, T);
}

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}
