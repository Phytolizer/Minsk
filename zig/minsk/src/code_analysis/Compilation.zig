const std = @import("std");
const SyntaxTree = @import("syntax/SyntaxTree.zig");
const Object = @import("minsk_runtime").Object;
const Binder = @import("binding/Binder.zig");
const Evaluator = @import("Evaluator.zig");

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
    failure: [][]const u8,
    success: Object,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            .failure => |diagnostics| {
                for (diagnostics) |d| {
                    allocator.free(d);
                }
                allocator.free(diagnostics);
            },
            .success => {},
        }
    }
};

pub fn evaluate(self: *Self) !EvaluationResult {
    var binder = Binder.init(self.allocator);
    const bound_expression = try binder.bindExpression(self.syntax_tree.root);
    defer bound_expression.deinit(self.allocator);
    const diagnostics = blk: {
        const slices = [_][][]const u8{
            self.syntax_tree.takeDiagnostics(),
            try binder.diagnostics.toOwnedSlice(),
        };
        defer for (slices) |s| {
            self.allocator.free(s);
        };
        break :blk try std.mem.concat(self.allocator, []const u8, &slices);
    };
    if (diagnostics.len > 0) {
        return .{ .failure = diagnostics };
    }

    var evaluator = Evaluator.init(bound_expression);
    return .{ .success = evaluator.evaluate() };
}
