const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;

base: SyntaxNode,

const Self = @This();

pub fn init(
    kind: SyntaxKind,
    deinit_fn: SyntaxNode.DeinitFn,
    children_fn: SyntaxNode.ChildrenFn,
) Self {
    return .{
        .base = .{
            .kind = kind,
            .deinit_fn = deinit_fn,
            .children_fn = children_fn,
        },
    };
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.base.deinit(allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]*const SyntaxNode {
    return try self.base.children(allocator);
}

pub fn downcast(base: anytype, comptime T: type) DowncastedPointer(@TypeOf(base), T) {
    const self = @fieldParentPtr(Self, "base", base);
    return @fieldParentPtr(T, "base", self);
}
