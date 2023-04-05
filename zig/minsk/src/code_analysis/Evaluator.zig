const std = @import("std");
const BoundExpression = @import("binding/BoundExpression.zig");
const BoundAssignmentExpression = @import("binding/BoundAssignmentExpression.zig");
const BoundBinaryExpression = @import("binding/BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("binding/BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("binding/BoundUnaryExpression.zig");
const BoundVariableExpression = @import("binding/BoundVariableExpression.zig");
const Object = @import("minsk_runtime").Object;
const VariableSymbol = @import("VariableSymbol.zig");

root: *const BoundExpression,
variables: *VariableSymbol.Map,

const Self = @This();

pub fn init(
    root: *const BoundExpression,
    variables: *VariableSymbol.Map,
) Self {
    return .{
        .root = root,
        .variables = variables,
    };
}

pub fn evaluate(self: Self) !Object {
    return try self.evaluateExpression(self.root);
}

fn evaluateExpression(self: Self, node: *const BoundExpression) std.mem.Allocator.Error!Object {
    return switch (node.base.kind) {
        .assignment_expression => try self.evaluateAssignmentExpression(
            BoundExpression.downcast(node, BoundAssignmentExpression),
        ),
        .binary_expression => try self.evaluateBinaryExpression(
            BoundExpression.downcast(node, BoundBinaryExpression),
        ),
        .literal_expression => try self.evaluateLiteralExpression(
            BoundExpression.downcast(node, BoundLiteralExpression),
        ),
        .unary_expression => try self.evaluateUnaryExpression(
            BoundExpression.downcast(node, BoundUnaryExpression),
        ),
        .variable_expression => try self.evaluateVariableExpression(
            BoundExpression.downcast(node, BoundVariableExpression),
        ),
    };
}

fn evaluateAssignmentExpression(self: Self, node: *const BoundAssignmentExpression) !Object {
    const value = try self.evaluateExpression(node.expression);
    try self.variables.put(node.variable, value);
    return value;
}

fn evaluateBinaryExpression(self: Self, node: *const BoundBinaryExpression) !Object {
    const left = try self.evaluateExpression(node.left);
    const right = try self.evaluateExpression(node.right);

    return switch (node.operator.kind) {
        .addition => .{ .integer = left.integer + right.integer },
        .subtraction => .{ .integer = left.integer - right.integer },
        .multiplication => .{ .integer = left.integer *% right.integer },
        .division => .{ .integer = @divTrunc(left.integer, right.integer) },
        .logical_and => .{ .boolean = left.boolean and right.boolean },
        .logical_or => .{ .boolean = left.boolean or right.boolean },
        .equality => .{ .boolean = left.eq(right) },
        .inequality => .{ .boolean = !left.eq(right) },
    };
}

fn evaluateLiteralExpression(_: Self, node: *const BoundLiteralExpression) !Object {
    return node.value;
}

fn evaluateUnaryExpression(self: Self, node: *const BoundUnaryExpression) !Object {
    const operand = try self.evaluateExpression(node.operand);

    return switch (node.operator.kind) {
        .identity => .{ .integer = operand.integer },
        .negation => .{ .integer = -operand.integer },
        .logical_negation => .{ .boolean = !operand.boolean },
    };
}

fn evaluateVariableExpression(self: Self, node: *const BoundVariableExpression) !Object {
    return self.variables.get(node.variable).?.?;
}
