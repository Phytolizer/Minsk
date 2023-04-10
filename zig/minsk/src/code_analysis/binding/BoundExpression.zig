const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const Object = @import("minsk_runtime").Object;

pub const TypeFn = *const fn (self: *const Self) Object.Type;

base: BoundNode,
type_fn: TypeFn,

const Self = @This();

pub fn init(
    kind: BoundNodeKind,
    deinit_fn: BoundNode.DeinitFn,
    children_fn: BoundNode.ChildrenFn,
    type_fn: TypeFn,
) Self {
    return .{
        .base = .{
            .kind = kind,
            .deinit_fn = deinit_fn,
            .children_fn = children_fn,
        },
        .type_fn = type_fn,
    };
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.base.deinit(allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]*const Self {
    return try self.base.children(allocator);
}

pub fn @"type"(self: *const Self) Object.Type {
    return self.type_fn(self);
}

pub fn downcastNode(base: anytype, comptime T: type) DowncastedPointer(@TypeOf(base), T) {
    const self = @fieldParentPtr(Self, "base", base);
    return @fieldParentPtr(T, "base", self);
}

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}
