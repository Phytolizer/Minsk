const std = @import("std");
const SyntaxTree = @import("syntax/SyntaxTree.zig");
const Object = @import("minsk_runtime").Object;
const Binder = @import("binding/Binder.zig");
const Evaluator = @import("Evaluator.zig");
const Diagnostic = @import("Diagnostic.zig");
const VariableSymbol = @import("VariableSymbol.zig");

allocator: std.mem.Allocator,
syntax_tree: SyntaxTree,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, syntax_tree: SyntaxTree) Self {
    return .{
        .allocator = allocator,
        .syntax_tree = syntax_tree,
    };
}

pub fn deinit(self: Self) void {
    self.syntax_tree.deinit();
}

pub const EvaluationResult = union(enum) {
    failure: []Diagnostic,
    success: Object,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            .failure => |diagnostics| {
                for (diagnostics) |d| {
                    d.deinit(allocator);
                }
                allocator.free(diagnostics);
            },
            .success => {},
        }
    }
};

pub fn evaluate(self: *Self, variables: *VariableSymbol.Map) !EvaluationResult {
    var binder = Binder.init(self.allocator, variables);
    const bound_expression = try binder.bindExpression(self.syntax_tree.root);
    defer bound_expression.deinit(self.allocator);
    const diagnostics = blk: {
        var diagnostics = self.syntax_tree.takeDiagnostics();
        try diagnostics.extend(&binder.diagnostics);
        binder.diagnostics.clear();
        break :blk try diagnostics.diagnostics.toOwnedSlice();
    };
    if (diagnostics.len > 0) {
        return .{ .failure = diagnostics };
    }

    var evaluator = Evaluator.init(bound_expression, variables);
    return .{ .success = try evaluator.evaluate() };
}
