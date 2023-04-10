const std = @import("std");
const SyntaxToken = @import("SyntaxToken.zig");
const Lexer = @import("Lexer.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("BinaryExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("UnaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("LiteralExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("ParenthesizedExpressionSyntax.zig");
const AssignmentExpressionSyntax = @import("AssignmentExpressionSyntax.zig");
const NameExpressionSyntax = @import("NameExpressionSyntax.zig");
const IfStatementSyntax = @import("IfStatementSyntax.zig");
const ElseClauseSyntax = @import("ElseClauseSyntax.zig");
const WhileStatementSyntax = @import("WhileStatementSyntax.zig");

const StatementSyntax = @import("StatementSyntax.zig");
const BlockStatementSyntax = @import("BlockStatementSyntax.zig");
const ExpressionStatementSyntax = @import("ExpressionStatementSyntax.zig");
const VariableDeclarationSyntax = @import("VariableDeclarationSyntax.zig");

const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const syntax_facts = @import("syntax_facts.zig");
const CompilationUnitSyntax = @import("CompilationUnitSyntax.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");
const SourceText = @import("../text/SourceText.zig");

const AllocError = std.mem.Allocator.Error;

allocator: std.mem.Allocator,
source: *const SourceText,
tokens: []SyntaxToken,
position: usize = 0,
diagnostics: DiagnosticBag,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, source: *const SourceText) !Self {
    var lexer = try Lexer.init(allocator, source);
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
        .source = source,
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

fn matchToken(self: *Self, kind: SyntaxKind) AllocError!SyntaxToken {
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

pub fn parseCompilationUnit(self: *Self) AllocError!*CompilationUnitSyntax {
    const statement = try self.parseStatement();
    const end_of_file_token = try self.matchToken(.end_of_file_token);
    return try CompilationUnitSyntax.init(
        self.allocator,
        statement,
        end_of_file_token,
    );
}

fn takeDiagnostics(self: *Self) DiagnosticBag {
    const result = self.diagnostics;
    self.diagnostics = DiagnosticBag.init(self.allocator);
    return result;
}

fn parseStatement(self: *Self) AllocError!*StatementSyntax {
    return switch (self.current().kind) {
        .open_brace_token => try self.parseBlockStatement(),
        .let_keyword, .var_keyword => try self.parseVariableDeclaration(),
        .if_keyword => try self.parseIfStatement(),
        .while_keyword => try self.parseWhileStatement(),
        else => try self.parseExpressionStatement(),
    };
}

fn parseBlockStatement(self: *Self) AllocError!*StatementSyntax {
    var statements = std.ArrayList(*StatementSyntax).init(self.allocator);

    const open_brace_token = try self.matchToken(.open_brace_token);
    while (self.current().kind != .end_of_file_token and
        self.current().kind != .close_brace_token)
    {
        const statement = try self.parseStatement();
        try statements.append(statement);
    }
    const close_brace_token = try self.matchToken(.close_brace_token);
    return try BlockStatementSyntax.init(
        self.allocator,
        open_brace_token,
        try statements.toOwnedSlice(),
        close_brace_token,
    );
}

fn parseIfStatement(self: *Self) AllocError!*StatementSyntax {
    const keyword_token = try self.matchToken(.if_keyword);
    const condition = try self.parseExpression();
    const then_statement = try self.parseStatement();
    const else_clause = try self.parseOptionalElseClause();
    return try IfStatementSyntax.init(
        self.allocator,
        keyword_token,
        condition,
        then_statement,
        else_clause,
    );
}

fn parseWhileStatement(self: *Self) AllocError!*StatementSyntax {
    const keyword_token = try self.matchToken(.while_keyword);
    const condition = try self.parseExpression();
    const body = try self.parseStatement();
    return try WhileStatementSyntax.init(
        self.allocator,
        keyword_token,
        condition,
        body,
    );
}

fn parseOptionalElseClause(self: *Self) AllocError!?*ElseClauseSyntax {
    if (self.current().kind != .else_keyword)
        return null;

    const keyword_token = self.nextToken();
    const statement = try self.parseStatement();
    return try ElseClauseSyntax.init(self.allocator, keyword_token, statement);
}

fn parseVariableDeclaration(self: *Self) AllocError!*StatementSyntax {
    const expected: SyntaxKind = switch (self.current().kind) {
        .let_keyword => .let_keyword,
        else => .var_keyword,
    };
    const keyword_token = try self.matchToken(expected);
    const identifier_token = try self.matchToken(.identifier_token);
    const equals_token = try self.matchToken(.equals_token);
    const initializer = try self.parseExpression();
    return try VariableDeclarationSyntax.init(
        self.allocator,
        keyword_token,
        identifier_token,
        equals_token,
        initializer,
    );
}

fn parseExpressionStatement(self: *Self) AllocError!*StatementSyntax {
    const expression = try self.parseExpression();
    return try ExpressionStatementSyntax.init(self.allocator, expression);
}

fn parseExpression(self: *Self) AllocError!*ExpressionSyntax {
    return try self.parseAssignmentExpression();
}

fn parseAssignmentExpression(self: *Self) !*ExpressionSyntax {
    if (self.peek(0).kind == .identifier_token and
        self.peek(1).kind == .equals_token)
    {
        const identifier_token = self.nextToken();
        const equals_token = self.nextToken();
        const expression = try self.parseAssignmentExpression();
        return try AssignmentExpressionSyntax.init(self.allocator, identifier_token, equals_token, expression);
    }

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
    return switch (self.current().kind) {
        .open_parenthesis_token => try self.parseParenthesizedExpression(),
        .false_keyword, .true_keyword => try self.parseBooleanLiteral(),
        .number_token => try self.parseNumberLiteral(),
        else => try self.parseNameExpression(),
    };
}

fn parseParenthesizedExpression(self: *Self) !*ExpressionSyntax {
    const left = try self.matchToken(.open_parenthesis_token);
    const expression = try self.parseExpression();
    errdefer expression.deinit(self.allocator);
    const right = try self.matchToken(.close_parenthesis_token);
    return try ParenthesizedExpressionSyntax.init(
        self.allocator,
        left,
        expression,
        right,
    );
}

fn parseBooleanLiteral(self: *Self) !*ExpressionSyntax {
    const value = self.current().kind == .true_keyword;
    const keyword_token = if (value)
        self.nextToken()
    else
        try self.matchToken(.false_keyword);
    return try LiteralExpressionSyntax.init(
        self.allocator,
        keyword_token,
        .{ .boolean = value },
    );
}

fn parseNameExpression(self: *Self) !*ExpressionSyntax {
    const identifier_token = try self.matchToken(.identifier_token);
    return try NameExpressionSyntax.init(self.allocator, identifier_token);
}

fn parseNumberLiteral(self: *Self) !*ExpressionSyntax {
    const number_token = try self.matchToken(.number_token);
    return try LiteralExpressionSyntax.init(
        self.allocator,
        number_token,
        number_token.value orelse .{ .integer = 0 },
    );
}
