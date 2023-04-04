const std = @import("std");
const SyntaxToken = @import("SyntaxToken.zig");
const Lexer = @import("Lexer.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("BinaryExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("UnaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("LiteralExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("ParenthesizedExpressionSyntax.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const syntax_facts = @import("syntax_facts.zig");
const SyntaxTree = @import("SyntaxTree.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");

allocator: std.mem.Allocator,
tokens: []SyntaxToken,
position: usize = 0,
diagnostics: DiagnosticBag,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
    var lexer = try Lexer.init(allocator, text);
    defer lexer.deinit();
    var tokens = std.ArrayList(SyntaxToken).init(allocator);
    defer tokens.deinit();

    while (try lexer.lex()) |token| {
        if (token.kind == .whitespace_token or token.kind == .bad_token)
            continue;
        try tokens.append(token);
    }
    var diagnostics = DiagnosticBag.init(allocator);
    try diagnostics.extend(&lexer.diagnostics);

    return .{
        .allocator = allocator,
        .tokens = try tokens.toOwnedSlice(),
        .diagnostics = diagnostics,
    };
}

pub fn deinit(self: Self) void {
    self.allocator.free(self.tokens);
    self.diagnostics.deinit();
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

fn matchToken(self: *Self, kind: SyntaxKind) std.mem.Allocator.Error!SyntaxToken {
    if (self.current().kind == kind) {
        return self.nextToken();
    }

    try self.diagnostics.reportUnexpectedToken(
        self.current().span(),
        self.current().kind,
        kind,
    );
    return SyntaxToken.init(
        kind,
        self.current().position,
        "",
        null,
    );
}

pub fn parse(self: *Self) std.mem.Allocator.Error!SyntaxTree {
    const expression = try self.parseExpression();
    const end_of_file_token = try self.matchToken(.end_of_file_token);
    return SyntaxTree.init(
        self.allocator,
        self.takeDiagnostics(),
        expression,
        end_of_file_token,
    );
}

fn takeDiagnostics(self: *Self) DiagnosticBag {
    const result = self.diagnostics;
    self.diagnostics = DiagnosticBag.init(self.allocator);
    return result;
}

fn parseExpression(self: *Self) std.mem.Allocator.Error!*ExpressionSyntax {
    return try self.parseBinaryExpression(0);
}

fn parseBinaryExpression(self: *Self, parent_precedence: usize) !*ExpressionSyntax {
    const unary_operator_precedence = syntax_facts.unaryOperatorPrecedence(self.current().kind);
    var left = if (unary_operator_precedence != 0 and unary_operator_precedence >= parent_precedence) blk: {
        const operator_token = self.nextToken();
        const operand = try self.parseBinaryExpression(unary_operator_precedence);
        break :blk try UnaryExpressionSyntax.init(self.allocator, operator_token, operand);
    } else try self.parsePrimaryExpression();

    errdefer left.deinit(self.allocator);

    while (true) {
        const precedence = syntax_facts.binaryOperatorPrecedence(self.current().kind);
        if (precedence == 0 or precedence <= parent_precedence)
            break;

        const operator_token = self.nextToken();
        const right = try self.parseBinaryExpression(precedence);
        errdefer right.deinit(self.allocator);
        left = try BinaryExpressionSyntax.init(self.allocator, left, operator_token, right);
    }

    return left;
}

fn parsePrimaryExpression(self: *Self) !*ExpressionSyntax {
    switch (self.current().kind) {
        .open_parenthesis_token => {
            const left = self.nextToken();
            const expression = try self.parseExpression();
            errdefer expression.deinit(self.allocator);
            const right = try self.matchToken(.close_parenthesis_token);
            return try ParenthesizedExpressionSyntax.init(
                self.allocator,
                left,
                expression,
                right,
            );
        },
        .false_keyword, .true_keyword => {
            const keyword_token = self.nextToken();
            const value = keyword_token.kind == .true_keyword;
            return try LiteralExpressionSyntax.init(
                self.allocator,
                keyword_token,
                .{ .boolean = value },
            );
        },
        else => {
            const number_token = try self.matchToken(.number_token);
            return try LiteralExpressionSyntax.init(
                self.allocator,
                number_token,
                number_token.value orelse .{ .integer = 0 },
            );
        },
    }
}
