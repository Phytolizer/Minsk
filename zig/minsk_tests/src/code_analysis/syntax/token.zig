const std = @import("std");
const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;

pub const SimpleToken = struct {
    kind: SyntaxKind,
    text: []const u8,
    pub fn init(kind: SyntaxKind, text: []const u8) @This() {
        return .{ .kind = kind, .text = text };
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("({s} '{'}')", .{ self.kind.displayName(), std.zig.fmtEscapes(self.text) });
    }
};

const st = SimpleToken.init;
const fixed_tokens = blk: {
    var result: []const SimpleToken = &.{};
    for (std.meta.tags(SyntaxKind)) |kind| {
        if (syntax_facts.getText(kind)) |text| {
            result = result ++ &[_]SimpleToken{st(kind, text)};
        }
    }
    break :blk result;
};

pub const simple = fixed_tokens ++ &[_]SimpleToken{
    st(.identifier_token, "a"),
    st(.identifier_token, "abc"),
    st(.number_token, "1"),
    st(.number_token, "123"),
};

pub const separators = &[_]SimpleToken{
    st(.whitespace_token, " "),
    st(.whitespace_token, "  "),
    st(.whitespace_token, "\r"),
    st(.whitespace_token, "\n"),
    st(.whitespace_token, "\r\n"),
};
