const std = @import("std");
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const BoundBlockStatement = @import("BoundBlockStatement.zig");
const BoundVariableDeclaration = @import("BoundVariableDeclaration.zig");
const BoundIfStatement = @import("BoundIfStatement.zig");
const BoundWhileStatement = @import("BoundWhileStatement.zig");
const BoundForStatement = @import("BoundForStatement.zig");
const BoundExpressionStatement = @import("BoundExpressionStatement.zig");
const BoundLiteralExpression = @import("BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("BoundUnaryExpression.zig");
const BoundBinaryExpression = @import("BoundBinaryExpression.zig");
const BoundAssignmentExpression = @import("BoundAssignmentExpression.zig");
const BoundVariableExpression = @import("BoundVariableExpression.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");
const BoundNode = @import("BoundNode.zig");

const AllocError = std.mem.Allocator.Error;

pub const RewriteFns = struct {
    fn Rewriter(comptime Node: type) type {
        const Base = std.meta.fieldInfo(Node, .base).type;
        return *const fn (self: *Self, node: *Node) AllocError!*Base;
    }

    pub const BlockStatement = Rewriter(BoundBlockStatement);
    pub const VariableDeclaration = Rewriter(BoundVariableDeclaration);
    pub const IfStatement = Rewriter(BoundIfStatement);
    pub const WhileStatement = Rewriter(BoundWhileStatement);
    pub const ForStatement = Rewriter(BoundForStatement);
    pub const ExpressionStatement = Rewriter(BoundExpressionStatement);

    pub const LiteralExpression = Rewriter(BoundLiteralExpression);
    pub const UnaryExpression = Rewriter(BoundUnaryExpression);
    pub const BinaryExpression = Rewriter(BoundBinaryExpression);
    pub const AssignmentExpression = Rewriter(BoundAssignmentExpression);
    pub const VariableExpression = Rewriter(BoundVariableExpression);

    block_statement: ?BlockStatement = null,
    variable_declaration: ?VariableDeclaration = null,
    if_statement: ?IfStatement = null,
    while_statement: ?WhileStatement = null,
    for_statement: ?ForStatement = null,
    expression_statement: ?ExpressionStatement = null,

    literal_expression: ?LiteralExpression = null,
    unary_expression: ?UnaryExpression = null,
    binary_expression: ?BinaryExpression = null,
    assignment_expression: ?AssignmentExpression = null,
    variable_expression: ?VariableExpression = null,
};

comptime {
    const rewrite_fn_names = std.meta.fieldNames(RewriteFns);
    const kind_names = std.meta.fieldNames(BoundNodeKind);

    for (kind_names) |kind_name| {
        blk: {
            for (rewrite_fn_names) |fn_name| {
                if (std.mem.eql(u8, fn_name, kind_name)) {
                    break :blk;
                }
            }
            @compileError(std.fmt.comptimePrint("Missing rewrite function for kind: {s}", .{kind_name}));
        }
    }
}

const Self = @This();

pub const DeinitFn = *const fn (self: *const Self) void;

rewrite_fns: RewriteFns,
deinit_fn: ?DeinitFn,
allocator: std.mem.Allocator,

pub fn deinit(self: *const Self) void {
    if (self.deinit_fn) |f| {
        f(self);
    }
}

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}

pub fn rewriteStatement(self: *Self, node: *BoundStatement) AllocError!*BoundStatement {
    switch (node.base.kind) {
        .block_statement => {
            const f = self.rewrite_fns.block_statement orelse rewriteBlockStatement;
            return try f(self, node.downcast(BoundBlockStatement));
        },
        .variable_declaration => {
            const f = self.rewrite_fns.variable_declaration orelse rewriteVariableDeclaration;
            return try f(self, node.downcast(BoundVariableDeclaration));
        },
        .if_statement => {
            const f = self.rewrite_fns.if_statement orelse rewriteIfStatement;
            return try f(self, node.downcast(BoundIfStatement));
        },
        .while_statement => {
            const f = self.rewrite_fns.while_statement orelse rewriteWhileStatement;
            return try f(self, node.downcast(BoundWhileStatement));
        },
        .for_statement => {
            const f = self.rewrite_fns.for_statement orelse rewriteForStatement;
            return try f(self, node.downcast(BoundForStatement));
        },
        .expression_statement => {
            const f = self.rewrite_fns.expression_statement orelse rewriteExpressionStatement;
            return try f(self, node.downcast(BoundExpressionStatement));
        },
        else => unreachable,
    }
}

fn rewriteBlockStatement(self: *Self, node: *BoundBlockStatement) AllocError!*BoundStatement {
    var statements: ?std.ArrayList(*BoundStatement) = null;
    defer if (statements) |s| s.deinit();

    for (node.statements, 0..) |old_statement, i| {
        const new_statement = try self.rewriteStatement(old_statement);
        if (new_statement != old_statement) {
            old_statement.deinit(self.allocator);
            if (statements == null) {
                statements = try std.ArrayList(*BoundStatement).initCapacity(self.allocator, node.statements.len);
                for (0..i, node.statements) |_, statement| {
                    try statements.?.append(statement);
                }
            }
        }

        if (statements) |*s| {
            try s.append(new_statement);
        }
    }

    if (statements) |*s| {
        return try BoundBlockStatement.init(self.allocator, try s.toOwnedSlice());
    }

    return &node.base;
}

fn rewriteVariableDeclaration(self: *Self, node: *BoundVariableDeclaration) AllocError!*BoundStatement {
    const initializer = try self.rewriteExpression(node.initializer);
    return if (initializer == node.initializer)
        &node.base
    else
        try BoundVariableDeclaration.init(self.allocator, node.variable, initializer);
}

fn rewriteIfStatement(self: *Self, node: *BoundIfStatement) AllocError!*BoundStatement {
    const condition = try self.rewriteExpression(node.condition);
    const then_statement = try self.rewriteStatement(node.then_statement);
    const else_statement = if (node.else_statement) |else_statement|
        try self.rewriteStatement(else_statement)
    else
        null;

    return if (condition == node.condition and then_statement == node.then_statement and else_statement == node.else_statement)
        &node.base
    else
        try BoundIfStatement.init(self.allocator, condition, then_statement, else_statement);
}

fn rewriteWhileStatement(self: *Self, node: *BoundWhileStatement) AllocError!*BoundStatement {
    const condition = try self.rewriteExpression(node.condition);
    const body = try self.rewriteStatement(node.body);

    return if (condition == node.condition and body == node.body)
        &node.base
    else
        try BoundWhileStatement.init(self.allocator, condition, body);
}

fn rewriteForStatement(self: *Self, node: *BoundForStatement) AllocError!*BoundStatement {
    const lower_bound = try self.rewriteExpression(node.lower_bound);
    const upper_bound = try self.rewriteExpression(node.upper_bound);
    const body = try self.rewriteStatement(node.body);

    return if (lower_bound == node.lower_bound and upper_bound == node.upper_bound and body == node.body)
        &node.base
    else
        try BoundForStatement.init(
            self.allocator,
            node.variable,
            lower_bound,
            upper_bound,
            body,
        );
}

fn rewriteExpressionStatement(self: *Self, node: *BoundExpressionStatement) AllocError!*BoundStatement {
    const expression = try self.rewriteExpression(node.expression);
    return if (expression == node.expression)
        &node.base
    else
        try BoundExpressionStatement.init(self.allocator, expression);
}

pub fn rewriteExpression(self: *Self, node: *BoundExpression) AllocError!*BoundExpression {
    switch (node.base.kind) {
        .literal_expression => {
            const f = self.rewrite_fns.literal_expression orelse rewriteLiteralExpression;
            return try f(self, node.downcast(BoundLiteralExpression));
        },
        .unary_expression => {
            const f = self.rewrite_fns.unary_expression orelse rewriteUnaryExpression;
            return try f(self, node.downcast(BoundUnaryExpression));
        },
        .binary_expression => {
            const f = self.rewrite_fns.binary_expression orelse rewriteBinaryExpression;
            return try f(self, node.downcast(BoundBinaryExpression));
        },
        .assignment_expression => {
            const f = self.rewrite_fns.assignment_expression orelse rewriteAssignmentExpression;
            return try f(self, node.downcast(BoundAssignmentExpression));
        },
        .variable_expression => {
            const f = self.rewrite_fns.variable_expression orelse rewriteVariableExpression;
            return try f(self, node.downcast(BoundVariableExpression));
        },
        else => unreachable,
    }
}

fn rewriteLiteralExpression(self: *Self, node: *BoundLiteralExpression) AllocError!*BoundExpression {
    _ = self;
    return &node.base;
}

fn rewriteUnaryExpression(self: *Self, node: *BoundUnaryExpression) AllocError!*BoundExpression {
    const operand = try self.rewriteExpression(node.operand);
    return if (operand == node.operand)
        &node.base
    else
        try BoundUnaryExpression.init(self.allocator, node.operator, operand);
}

fn rewriteBinaryExpression(self: *Self, node: *BoundBinaryExpression) AllocError!*BoundExpression {
    const left = try self.rewriteExpression(node.left);
    const right = try self.rewriteExpression(node.right);
    return if (left == node.left and right == node.right)
        &node.base
    else
        try BoundBinaryExpression.init(self.allocator, left, node.operator, right);
}

fn rewriteAssignmentExpression(self: *Self, node: *BoundAssignmentExpression) AllocError!*BoundExpression {
    const expression = try self.rewriteExpression(node.expression);
    return if (expression == node.expression)
        &node.base
    else
        try BoundAssignmentExpression.init(self.allocator, node.variable, expression);
}

fn rewriteVariableExpression(self: *Self, node: *BoundVariableExpression) AllocError!*BoundExpression {
    _ = self;
    return &node.base;
}
