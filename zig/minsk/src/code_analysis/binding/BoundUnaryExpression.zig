const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const SyntaxKind = @import("../syntax/syntax_kind.zig").SyntaxKind;

base: BoundExpression,
operator: Operator,
operand: *BoundExpression,

pub const Operator = struct {
    syntax_kind: SyntaxKind,
    kind: Kind,
    operand_type: Object.Type,
    result_type: Object.Type,

    pub const Kind = enum {
        identity,
        negation,
        logical_negation,
        bitwise_negation,
    };

    fn initMatching(syntax_kind: SyntaxKind, kind: Kind, matching_type: Object.Type) Operator {
        return .{
            .syntax_kind = syntax_kind,
            .kind = kind,
            .operand_type = matching_type,
            .result_type = matching_type,
        };
    }

    const operators = [_]Operator{
        initMatching(.plus_token, .identity, .integer),
        initMatching(.minus_token, .negation, .integer),
        initMatching(.bang_token, .logical_negation, .boolean),
        initMatching(.tilde_token, .bitwise_negation, .integer),
    };

    pub fn bind(syntax_kind: SyntaxKind, operand_type: Object.Type) ?Operator {
        for (operators) |op| {
            if (op.syntax_kind == syntax_kind and
                op.operand_type == operand_type)
            {
                return op;
            }
        }
        return null;
    }
};

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    operator: Operator,
    operand: *BoundExpression,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.unary_expression, &deinit, &children, &properties, &@"type"),
        .operator = operator,
        .operand = operand,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    self.operand.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundExpression.downcastNode(node, Self);
    return try allocator.dupe(*const BoundNode, &.{
        &self.operand.base,
    });
}

fn properties(node: *const BoundNode, allocator: std.mem.Allocator) ![]BoundNode.Property {
    const self = BoundExpression.downcastNode(node, Self);
    return try allocator.dupe(BoundNode.Property, &.{.{
        .name = "operation",
        .value = try allocator.dupe(u8, @tagName(self.operator.kind)),
    }});
}

fn @"type"(node: *const BoundExpression) Object.Type {
    const self = BoundExpression.downcast(node, Self);
    return self.operator.result_type;
}
