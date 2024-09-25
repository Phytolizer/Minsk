const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");

base: BoundStatement,
expression: *BoundExpression,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    expression: *BoundExpression,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.expression_statement, &deinit, &children, null),
        .expression = expression,
    };
    return &result.base;
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundStatement.downcastNode(node, Self);
    return try allocator.dupe(*const BoundNode, &.{
        &self.expression.base,
    });
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    self.expression.deinit(allocator);
    allocator.destroy(self);
}
