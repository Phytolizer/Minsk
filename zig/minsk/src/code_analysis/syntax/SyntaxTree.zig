const std = @import("std");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");
const Parser = @import("Parser.zig");
const Lexer = @import("Lexer.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");

allocator: std.mem.Allocator,
diagnostics: ?DiagnosticBag,
root: *ExpressionSyntax,
end_of_file_token: SyntaxToken,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    diagnostics: DiagnosticBag,
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
    if (self.diagnostics) |diagnostics| {
        diagnostics.deinit();
    }
    self.root.deinit(self.allocator);
}

pub fn parse(allocator: std.mem.Allocator, text: []const u8) !Self {
    var parser = try Parser.init(allocator, text);
    defer parser.deinit();
    return try parser.parse();
}

pub fn parseTokens(allocator: std.mem.Allocator, text: []const u8) ![]SyntaxToken {
    var lexer = try Lexer.init(allocator, text);
    defer lexer.deinit();
    var tokens = std.ArrayList(SyntaxToken).init(allocator);
    while (try lexer.lex()) |tok| {
        if (tok.kind == .end_of_file_token) break;

        try tokens.append(tok);
    }
    return try tokens.toOwnedSlice();
}

pub fn takeDiagnostics(self: *Self) DiagnosticBag {
    const result = self.diagnostics.?;
    self.diagnostics = null;
    return result;
}
