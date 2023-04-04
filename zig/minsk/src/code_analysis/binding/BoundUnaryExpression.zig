const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const SyntaxKind = @import("../syntax/syntax_kind.zig").SyntaxKind;

base: BoundExpression,
operator_kind: OperatorKind,
operand: *BoundExpression,

pub const OperatorKind = enum {
    identity,
    negation,
    logical_negation,

    pub fn bind(syntax_kind: SyntaxKind, operand_type: Object.Type) ?OperatorKind {
        switch (operand_type) {
            .integer => switch (syntax_kind) {
                .plus_token => return .identity,
                .minus_token => return .negation,
                else => {},
            },
            .boolean => switch (syntax_kind) {
                .bang_token => return .logical_negation,
                else => {},
            },
        }
        return null;
    }
};

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    operator_kind: OperatorKind,
    operand: *BoundExpression,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.unary_expression, &deinit, &@"type"),
        .operator_kind = operator_kind,
        .operand = operand,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    self.operand.deinit(allocator);
    allocator.destroy(self);
}

fn @"type"(node: *const BoundExpression) Object.Type {
    const self = BoundExpression.downcast(node, Self);
    return self.operand.type();
}
