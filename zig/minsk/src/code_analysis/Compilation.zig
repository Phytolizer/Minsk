const std = @import("std");
const SyntaxTree = @import("syntax/SyntaxTree.zig");
const Object = @import("minsk_runtime").Object;
const Binder = @import("binding/Binder.zig");
const Evaluator = @import("Evaluator.zig");
const Diagnostic = @import("Diagnostic.zig");
const DiagnosticBag = @import("DiagnosticBag.zig");
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
    const global_scope = try Binder.bindGlobalScope(self.allocator, self.syntax_tree.root);
    defer global_scope.deinit();
    const diagnostics = blk: {
        var diagnostics = self.syntax_tree.takeDiagnostics();
        var global_scope_diagnostics = DiagnosticBag.init(self.allocator);
        std.mem.swap(DiagnosticBag, &global_scope.diagnostics, &global_scope_diagnostics);
        try diagnostics.extend(&global_scope_diagnostics);
        break :blk try diagnostics.diagnostics.toOwnedSlice();
    };
    if (diagnostics.len > 0) {
        return .{ .failure = diagnostics };
    }

    var evaluator = Evaluator.init(global_scope.expression, variables);
    return .{ .success = try evaluator.evaluate() };
}
