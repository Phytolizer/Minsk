const std = @import("std");
const CompilationUnitSyntax = @import("CompilationUnitSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");
const Parser = @import("Parser.zig");
const Lexer = @import("Lexer.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");
const SourceText = @import("../text/SourceText.zig");

allocator: std.mem.Allocator,
source: *const SourceText,
diagnostics: ?DiagnosticBag,
root: *CompilationUnitSyntax,

const Self = @This();

fn init(
    allocator: std.mem.Allocator,
    source: *const SourceText,
) !Self {
    var parser = try Parser.init(allocator, source);
    defer parser.deinit();
    const root = try parser.parseCompilationUnit();

    var diagnostics = DiagnosticBag.init(allocator);
    std.mem.swap(DiagnosticBag, &diagnostics, &parser.diagnostics);
    return .{
        .allocator = allocator,
        .source = source,
        .diagnostics = diagnostics,
        .root = root,
    };
}

pub fn deinit(self: Self) void {
    self.source.deinit();
    if (self.diagnostics) |diagnostics| {
        diagnostics.deinit();
    }
    self.root.base.deinit(self.allocator);
}

pub fn parse(allocator: std.mem.Allocator, text: []const u8) !Self {
    return try init(allocator, try SourceText.from(allocator, text));
}

fn parseTokensSource(allocator: std.mem.Allocator, source: *const SourceText) ![]SyntaxToken {
    defer source.deinit();
    var lexer = try Lexer.init(allocator, source);
    defer lexer.deinit();
    var tokens = std.ArrayList(SyntaxToken).init(allocator);
    while (try lexer.lex()) |tok| {
        if (tok.kind == .end_of_file_token) break;

        try tokens.append(tok);
    }
    return try tokens.toOwnedSlice();
}

pub fn parseTokens(allocator: std.mem.Allocator, text: []const u8) ![]SyntaxToken {
    return try parseTokensSource(allocator, try SourceText.from(allocator, text));
}

pub fn takeDiagnostics(self: *Self) DiagnosticBag {
    const result = self.diagnostics.?;
    self.diagnostics = null;
    return result;
}
