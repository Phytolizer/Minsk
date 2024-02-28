const std = @import("std");
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;

const AllocError = std.mem.Allocator.Error;

pub const Property = struct {
    name: []const u8,
    value: []const u8,
};

pub const DeinitFn = *const fn (self: *const Self, allocator: std.mem.Allocator) void;
pub const ChildrenFn = *const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]*const Self;
pub const PropertiesFn = ?*const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]Property;

kind: BoundNodeKind,
deinit_fn: DeinitFn,
children_fn: ChildrenFn,
properties_fn: PropertiesFn,

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

pub fn properties(self: *const Self, allocator: std.mem.Allocator) ![]Property {
    if (self.properties_fn) |properties_fn| {
        return try properties_fn(self, allocator);
    }
    return try allocator.dupe(Property, &.{});
}
