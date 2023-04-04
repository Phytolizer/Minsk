const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;

base: BoundExpression,
value: Object,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    value: Object,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.literal_expression, &deinit, &@"type"),
        .value = value,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    allocator.destroy(self);
}

fn @"type"(node: *const BoundExpression) Object.Type {
    const self = BoundExpression.downcast(node, Self);
    return std.meta.activeTag(self.value);
}
