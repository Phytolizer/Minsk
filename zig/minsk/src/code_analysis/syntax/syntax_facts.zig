const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;

pub fn binaryOperatorPrecedence(kind: SyntaxKind) usize {
    return switch (kind) {
        .plus_token, .minus_token => 1,
        .star_token, .slash_token => 2,
        else => 0,
    };
}

pub fn unaryOperatorPrecedence(kind: SyntaxKind) usize {
    return switch (kind) {
        .plus_token, .minus_token => 3,
        else => 0,
    };
}

fn streq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn keywordKind(text: []const u8) SyntaxKind {
    return if (streq(text, "true"))
        .true_keyword
    else if (streq(text, "false"))
        .false_keyword
    else
        .identifier_token;
}
