const std = @import("std");
const SyntaxTree = @import("syntax/SyntaxTree.zig");
const Object = @import("minsk_runtime").Object;
const Binder = @import("binding/Binder.zig");
const Evaluator = @import("Evaluator.zig");
const Diagnostic = @import("Diagnostic.zig");
const DiagnosticBag = @import("DiagnosticBag.zig");
const VariableSymbol = @import("VariableSymbol.zig");
const BoundGlobalScope = @import("binding/BoundGlobalScope.zig");
const Atomic = std.atomic.Atomic;

allocator: std.mem.Allocator,
previous: ?*Self = null,
syntax_tree: SyntaxTree,
global_scope: Atomic(?*BoundGlobalScope) = Atomic(?*BoundGlobalScope).init(null),

const Self = @This();

pub fn init(allocator: std.mem.Allocator, syntax_tree: SyntaxTree) !*Self {
    return try initPrev(allocator, null, syntax_tree);
}

fn initPrev(allocator: std.mem.Allocator, previous: ?*Self, syntax_tree: SyntaxTree) !*Self {
    const result = try allocator.create(Self);
    result.* = .{
        .allocator = allocator,
        .previous = previous,
        .syntax_tree = syntax_tree,
    };
    return result;
}

pub fn deinit(
    self: *Self,
    comptime with_parents: enum { with_parents, without_parents },
) void {
    if (with_parents == .with_parents) if (self.previous) |p| p.deinit(.with_parents);
    self.syntax_tree.deinit();
    if (self.global_scope.load(.SeqCst)) |gs| gs.deinit();
    self.allocator.destroy(self);
}

fn globalScope(self: *Self) !*BoundGlobalScope {
    if (self.global_scope.load(.SeqCst)) |gs| return gs;

    const new_gs = try Binder.bindGlobalScope(
        self.allocator,
        if (self.previous) |p| try p.globalScope() else null,
        self.syntax_tree.root,
    );
    if (self.global_scope.compareAndSwap(null, new_gs, .SeqCst, .SeqCst)) |old_gs| {
        new_gs.deinit();
        // not null, compareAndSwap would not give anything if it was
        return old_gs.?;
    }
    return new_gs;
}

pub fn continueWith(self: *Self, syntax_tree: SyntaxTree) !*Self {
    return try initPrev(self.allocator, self, syntax_tree);
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
    const global_scope = try self.globalScope();
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
