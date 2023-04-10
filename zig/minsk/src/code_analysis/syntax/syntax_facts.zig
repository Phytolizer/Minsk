const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;

pub fn binaryOperatorPrecedence(kind: SyntaxKind) usize {
    return switch (kind) {
        .pipe_pipe_token => 1,
        .ampersand_ampersand_token => 2,
        .equals_equals_token,
        .bang_equals_token,
        .less_token,
        .less_equals_token,
        .greater_token,
        .greater_equals_token,
        => 3,
        .plus_token, .minus_token => 4,
        .star_token, .slash_token => 5,
        else => 0,
    };
}

pub fn unaryOperatorPrecedence(kind: SyntaxKind) usize {
    return switch (kind) {
        .plus_token, .minus_token, .bang_token => 6,
        else => 0,
    };
}

fn streq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn keywordKind(text: []const u8) SyntaxKind {
    const keywords = std.ComptimeStringMap(SyntaxKind, .{
        .{ "true", .true_keyword },
        .{ "false", .false_keyword },
        .{ "let", .let_keyword },
        .{ "var", .var_keyword },
        .{ "if", .if_keyword },
        .{ "else", .else_keyword },
        .{ "while", .while_keyword },
        .{ "for", .for_keyword },
        .{ "to", .to_keyword },
    });
    return keywords.get(text) orelse .identifier_token;
}

pub fn getText(kind: SyntaxKind) ?[]const u8 {
    return switch (kind) {
        .plus_token => "+",
        .minus_token => "-",
        .star_token => "*",
        .slash_token => "/",
        .bang_token => "!",
        .equals_token => "=",
        .less_token => "<",
        .less_equals_token => "<=",
        .greater_token => ">",
        .greater_equals_token => ">=",
        .ampersand_ampersand_token => "&&",
        .pipe_pipe_token => "||",
        .equals_equals_token => "==",
        .bang_equals_token => "!=",
        .open_parenthesis_token => "(",
        .close_parenthesis_token => ")",
        .open_brace_token => "{",
        .close_brace_token => "}",
        .false_keyword => "false",
        .true_keyword => "true",
        .let_keyword => "let",
        .var_keyword => "var",
        .if_keyword => "if",
        .else_keyword => "else",
        .while_keyword => "while",
        .for_keyword => "for",
        .to_keyword => "to",
        else => null,
    };
}

pub fn binaryOperators() []const SyntaxKind {
    return comptime blk: {
        var result: []const SyntaxKind = &.{};
        for (std.meta.tags(SyntaxKind)) |k| {
            if (binaryOperatorPrecedence(k) > 0) {
                result = result ++ &[_]SyntaxKind{k};
            }
        }
        break :blk result;
    };
}

pub fn unaryOperators() []const SyntaxKind {
    return comptime blk: {
        var result: []const SyntaxKind = &.{};
        for (std.meta.tags(SyntaxKind)) |k| {
            if (unaryOperatorPrecedence(k) > 0) {
                result = result ++ &[_]SyntaxKind{k};
            }
        }
        break :blk result;
    };
}
