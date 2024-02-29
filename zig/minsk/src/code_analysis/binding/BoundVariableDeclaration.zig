const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");
const VariableSymbol = @import("../VariableSymbol.zig");

base: BoundStatement,
variable: VariableSymbol,
initializer: *BoundExpression,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    variable: VariableSymbol,
    initializer: *BoundExpression,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.variable_declaration, &deinit, &children, &properties),
        .variable = variable,
        .initializer = initializer,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    self.initializer.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundStatement.downcastNode(node, Self);
    return try allocator.dupe(*const BoundNode, &.{
        &self.initializer.base,
    });
}

fn properties(node: *const BoundNode, allocator: std.mem.Allocator) ![]BoundNode.Property {
    const self = BoundStatement.downcastNode(node, Self);
    return try allocator.dupe(BoundNode.Property, &[_]BoundNode.Property{.{
        .name = "variable",
        .value = try allocator.dupe(u8, self.variable.name),
    }});
}
