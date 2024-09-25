const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const Object = @import("minsk_runtime").Object;

base: BoundNode,

const Self = @This();

pub fn init(
    kind: BoundNodeKind,
    deinit_fn: BoundNode.DeinitFn,
    children_fn: BoundNode.ChildrenFn,
    properties_fn: BoundNode.PropertiesFn,
) Self {
    return .{
        .base = .{
            .kind = kind,
            .deinit_fn = deinit_fn,
            .children_fn = children_fn,
            .properties_fn = properties_fn,
        },
    };
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.base.deinit(allocator);
}

pub fn downcastNode(base: anytype, comptime T: type) DowncastedPointer(@TypeOf(base), T) {
    const self: DowncastedPointer(@TypeOf(base), Self) = @alignCast(@fieldParentPtr("base", base));
    return @alignCast(@fieldParentPtr("base", self));
}

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @alignCast(@fieldParentPtr("base", self));
}
