const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");

base: BoundStatement,
condition: *BoundExpression,
then_statement: *BoundStatement,
else_statement: ?*BoundStatement,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    condition: *BoundExpression,
    then_statement: *BoundStatement,
    else_statement: ?*BoundStatement,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.if_statement, &deinit, &children, &properties),
        .condition = condition,
        .then_statement = then_statement,
        .else_statement = else_statement,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    self.condition.deinit(allocator);
    self.then_statement.deinit(allocator);
    if (self.else_statement) |es| es.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundStatement.downcastNode(node, Self);
    var result = try std.ArrayList(*const BoundNode).initCapacity(allocator, 3);
    defer result.deinit();
    result.appendAssumeCapacity(&self.condition.base);
    result.appendAssumeCapacity(&self.then_statement.base);
    if (self.else_statement) |es|
        result.appendAssumeCapacity(&es.base);
    return try result.toOwnedSlice();
}

fn properties(node: *const BoundNode, allocator: std.mem.Allocator) ![]BoundNode.Property {
    _ = node;
    return try allocator.dupe(BoundNode.Property, &.{});
}
