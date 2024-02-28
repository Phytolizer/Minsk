const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const VariableSymbol = @import("../VariableSymbol.zig");

base: BoundExpression,
variable: VariableSymbol,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    variable: VariableSymbol,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.variable_expression, &deinit, &children, &properties, &@"type"),
        .variable = variable,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    allocator.destroy(self);
}

fn children(_: *const BoundNode, _: std.mem.Allocator) ![]*const BoundNode {
    return &.{};
}

fn properties(node: *const BoundNode, allocator: std.mem.Allocator) ![]BoundNode.Property {
    const self = BoundExpression.downcastNode(node, Self);
    return try allocator.dupe(BoundNode.Property, &[_]BoundNode.Property{.{
        .name = "variable",
        .value = self.variable.name,
    }});
}

fn @"type"(node: *const BoundExpression) Object.Type {
    const self = BoundExpression.downcast(node, Self);
    return self.variable.ty;
}
