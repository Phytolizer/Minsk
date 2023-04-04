const std = @import("std");
const SyntaxToken = @import("SyntaxToken.zig");
const Lexer = @import("Lexer.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("BinaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("LiteralExpressionSyntax.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;

allocator: std.mem.Allocator,
tokens: []SyntaxToken,
position: usize = 0,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
    var lexer = try Lexer.init(allocator, text);
    defer lexer.deinit();
    var tokens = std.ArrayList(SyntaxToken).init(allocator);
    defer tokens.deinit();

    while (try lexer.nextToken()) |token| {
        if (token.kind == .whitespace_token or token.kind == .end_of_file_token)
            continue;
        try tokens.append(token);
    }

    return .{
        .allocator = allocator,
        .tokens = try tokens.toOwnedSlice(),
    };
}

pub fn deinit(self: Self) void {
    self.allocator.free(self.tokens);
}

fn peek(self: Self, offset: usize) SyntaxToken {
    const index = self.position + offset;
    if (index >= self.tokens.len) {
        return self.tokens[self.tokens.len - 1];
    }
    return self.tokens[index];
}

fn current(self: Self) SyntaxToken {
    return self.peek(0);
}

fn nextToken(self: *Self) SyntaxToken {
    const result = self.current();
    self.position += 1;
    return result;
}

fn matchToken(self: *Self, kind: SyntaxKind) SyntaxToken {
    if (self.current().kind == kind) {
        return self.nextToken();
    }

    return SyntaxToken.init(
        kind,
        self.current().position,
        "",
        null,
    );
}

pub fn parse(self: *Self) !*ExpressionSyntax {
    var left = try self.parsePrimaryExpression();
    errdefer left.deinit(self.allocator);

    while (self.current().kind == .plus_token or self.current().kind == .minus_token) {
        const operator_token = self.nextToken();
        const right = try self.parsePrimaryExpression();
        errdefer right.deinit(self.allocator);
        left = try BinaryExpressionSyntax.init(self.allocator, left, operator_token, right);
    }

    return left;
}

fn parsePrimaryExpression(self: *Self) !*ExpressionSyntax {
    const number_token = self.matchToken(.number_token);
    return try LiteralExpressionSyntax.init(self.allocator, number_token);
}
