const std = @import("std");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

allocator: std.mem.Allocator,
diagnostics: [][]const u8,
root: *ExpressionSyntax,
end_of_file_token: SyntaxToken,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    diagnostics: [][]const u8,
    root: *ExpressionSyntax,
    end_of_file_token: SyntaxToken,
) Self {
    return .{
        .allocator = allocator,
        .diagnostics = diagnostics,
        .root = root,
        .end_of_file_token = end_of_file_token,
    };
}

pub fn deinit(self: Self) void {
    for (self.diagnostics) |d| {
        self.allocator.free(d);
    }
    self.allocator.free(self.diagnostics);
    self.root.deinit(self.allocator);
}
