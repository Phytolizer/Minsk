const std = @import("std");
const BoundScope = @import("BoundScope.zig");
const BoundExpression = @import("BoundExpression.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");
const VariableSymbol = @import("../VariableSymbol.zig");

allocator: std.mem.Allocator,
previous: ?*const BoundGlobalScope,
diagnostics: DiagnosticBag,
variables: []const VariableSymbol,
expression: *BoundExpression,

const BoundGlobalScope = @This();

pub fn init(
    allocator: std.mem.Allocator,
    previous: ?*const BoundGlobalScope,
    diagnostics: DiagnosticBag,
    variables: []const VariableSymbol,
    expression: *BoundExpression,
) !*BoundGlobalScope {
    const result = try allocator.create(BoundGlobalScope);
    result.* = .{
        .allocator = allocator,
        .previous = previous,
        .diagnostics = diagnostics,
        .variables = variables,
        .expression = expression,
    };
    return result;
}

pub fn deinit(self: *const BoundGlobalScope) void {
    if (self.previous) |p| p.deinit();

    self.diagnostics.deinit();
    self.expression.deinit(self.allocator);
    for (self.variables) |v| {
        v.deinit(self.allocator);
    }
    self.allocator.free(self.variables);
    self.allocator.destroy(self);
}
