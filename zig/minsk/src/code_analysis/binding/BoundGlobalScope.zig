const std = @import("std");
const BoundScope = @import("BoundScope.zig");
const BoundStatement = @import("BoundStatement.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");
const VariableSymbol = @import("../VariableSymbol.zig");

allocator: std.mem.Allocator,
previous: ?*const BoundGlobalScope,
diagnostics: DiagnosticBag,
variables: []const VariableSymbol,
statement: *BoundStatement,

const BoundGlobalScope = @This();

pub fn init(
    allocator: std.mem.Allocator,
    previous: ?*const BoundGlobalScope,
    diagnostics: DiagnosticBag,
    variables: []const VariableSymbol,
    statement: *BoundStatement,
) !*BoundGlobalScope {
    const result = try allocator.create(BoundGlobalScope);
    result.* = .{
        .allocator = allocator,
        .previous = previous,
        .diagnostics = diagnostics,
        .variables = variables,
        .statement = statement,
    };
    return result;
}

pub fn deinit(self: *const BoundGlobalScope) void {
    if (self.previous) |p| p.deinit();

    self.diagnostics.deinit();
    self.statement.deinit(self.allocator);
    for (self.variables) |v| {
        v.deinit(self.allocator);
    }
    self.allocator.free(self.variables);
    self.allocator.destroy(self);
}
