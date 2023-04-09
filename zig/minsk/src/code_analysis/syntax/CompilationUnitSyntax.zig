const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: SyntaxNode,
statement: *StatementSyntax,
end_of_file_token: SyntaxToken,

const CompilationUnitSyntax = @This();

pub fn init(
    allocator: std.mem.Allocator,
    statement: *StatementSyntax,
    end_of_file_token: SyntaxToken,
) !*CompilationUnitSyntax {
    const result = try allocator.create(CompilationUnitSyntax);
    result.* = .{
        .base = .{
            .kind = .compilation_unit,
            .deinit_fn = &deinit,
            .children_fn = &children,
            .span_fn = null,
        },
        .statement = statement,
        .end_of_file_token = end_of_file_token,
    };
    return result;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = SyntaxNode.downcast(node, CompilationUnitSyntax);
    self.statement.deinit(allocator);
    allocator.destroy(self);
}

const AllocError = std.mem.Allocator.Error;

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = SyntaxNode.downcast(node, CompilationUnitSyntax);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.statement.base,
        &self.end_of_file_token.base,
    });
}
