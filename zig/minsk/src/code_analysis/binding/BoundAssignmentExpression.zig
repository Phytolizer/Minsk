const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const VariableSymbol = @import("../VariableSymbol.zig");

base: BoundExpression,
variable: VariableSymbol,
expression: *BoundExpression,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    variable: VariableSymbol,
    expression: *BoundExpression,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.assignment_expression, &deinit, &children, &properties, &@"type"),
        .variable = variable,
        .expression = expression,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    self.expression.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundExpression.downcastNode(node, Self);
    return try allocator.dupe(*const BoundNode, &.{
        &self.expression.base,
    });
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
    return self.expression.type();
}
