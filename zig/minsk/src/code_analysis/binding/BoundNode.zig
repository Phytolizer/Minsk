const std = @import("std");
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;

const AllocError = std.mem.Allocator.Error;

pub const DeinitFn = *const fn (self: *const Self, allocator: std.mem.Allocator) void;
pub const ChildrenFn = *const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]*const Self;

kind: BoundNodeKind,
deinit_fn: DeinitFn,
children_fn: ChildrenFn,

const Self = @This();

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.deinit_fn(self, allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]*const Self {
    return try self.children_fn(self, allocator);
}
