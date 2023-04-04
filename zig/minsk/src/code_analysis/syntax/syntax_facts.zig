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
