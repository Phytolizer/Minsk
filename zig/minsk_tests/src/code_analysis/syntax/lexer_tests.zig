const std = @import("std");
const t = @import("framework");
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const SyntaxToken = @import("minsk").code_analysis.syntax.SyntaxToken;
const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;

const SimpleToken = struct {
    kind: SyntaxKind,
    text: []const u8,
    pub fn init(kind: SyntaxKind, text: []const u8) @This() {
        return .{ .kind = kind, .text = text };
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("({s} '{'}')", .{ self.kind.displayName(), std.zig.fmtEscapes(self.text) });
    }
};

fn testToken(
    state: *t.TestState,
    token: SyntaxToken,
    exp: SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    try t.assert(
        state,
        token.kind == exp.kind,
        "kind mismatch: {s} != {s}",
        .{ token.kind.displayName(), exp.kind.displayName() },
        out_msg,
    );
    try t.assert(
        state,
        std.mem.eql(u8, token.text, exp.text),
        "text mismatch: '{s}' != '{s}'",
        .{ token.text, exp.text },
        out_msg,
    );
}

fn testsAllTokens(
    state: *t.TestState,
    _: void,
    out_msg: *[]const u8,
) t.TestExit!void {
    const token_kinds = comptime blk: {
        var result: []const SyntaxKind = &.{};
        for (std.meta.tags(SyntaxKind)) |kind|
            if ((std.mem.endsWith(u8, kind.displayName(), "Keyword") or
                std.mem.endsWith(u8, kind.displayName(), "Token")) and
                (kind != .bad_token and kind != .end_of_file_token))
            {
                result = result ++ &[_]SyntaxKind{kind};
            };
        break :blk result;
    };

    const tested_token_kinds = comptime blk: {
        var result: []const SyntaxKind = &.{};
        for (simple_tokens ++ separators) |tok|
            result = result ++ &[_]SyntaxKind{tok.kind};
        break :blk result;
    };

    const messages = comptime blk: {
        var result: []const []const u8 = &.{};
        for (token_kinds) |tk| {
            if (std.mem.indexOfScalar(SyntaxKind, tested_token_kinds, tk) == null) {
                result = result ++ &[_][]const u8{std.fmt.comptimePrint("missing token kind: {s}", .{tk.displayName()})};
            }
        }
        break :blk result;
    };
    const shown_messages = std.mem.join(t.allocator, "\n> ", messages) catch unreachable;
    defer t.allocator.free(shown_messages);
    try t.assert(
        state,
        messages.len == 0,
        "\n> {s}",
        .{shown_messages},
        out_msg,
    );
}

fn lexesToken(
    state: *t.TestState,
    ctx: SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    const tokens = SyntaxTree.parseTokens(t.allocator, ctx.text) catch unreachable;
    defer t.allocator.free(tokens);

    const token = try t.assertSingle(
        state,
        SyntaxToken,
        tokens,
        "too many tokens ({d})",
        .{tokens.len},
        out_msg,
    );
    try testToken(state, token, ctx, out_msg);
}

fn lexesTokenPair(
    state: *t.TestState,
    ctx: [2]SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    const text = std.mem.concat(t.allocator, u8, &.{ ctx[0].text, ctx[1].text }) catch unreachable;
    defer t.allocator.free(text);
    const tokens = SyntaxTree.parseTokens(t.allocator, text) catch unreachable;
    defer t.allocator.free(tokens);

    try t.assert(
        state,
        tokens.len == 2,
        "incorrect token count ({d} != {d})",
        .{ tokens.len, 2 },
        out_msg,
    );

    try testToken(state, tokens[0], ctx[0], out_msg);
    try testToken(state, tokens[1], ctx[1], out_msg);
}

fn lexesTokenPairWithSeparator(
    state: *t.TestState,
    ctx: [3]SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    const text = std.mem.concat(
        t.allocator,
        u8,
        &.{ ctx[0].text, ctx[1].text, ctx[2].text },
    ) catch unreachable;
    defer t.allocator.free(text);
    const tokens = SyntaxTree.parseTokens(t.allocator, text) catch unreachable;
    defer t.allocator.free(tokens);

    try t.assert(
        state,
        tokens.len == 3,
        "incorrect token count ({d} != {d})",
        .{ tokens.len, 3 },
        out_msg,
    );

    try testToken(state, tokens[0], ctx[0], out_msg);
    try testToken(state, tokens[1], ctx[1], out_msg);
    try testToken(state, tokens[2], ctx[2], out_msg);
}

fn requiresSeparator(k1: SyntaxKind, k2: SyntaxKind) bool {
    const k1_kw = std.mem.endsWith(u8, k1.displayName(), "Keyword");
    const k2_kw = std.mem.endsWith(u8, k2.displayName(), "Keyword");
    return (k1 == .identifier_token or k1_kw) and (k2 == .identifier_token or k2_kw) or
        (k1 == .identifier_token or k1_kw) and (k2 == .number_token) or
        (k1 == .whitespace_token) and (k2 == .whitespace_token) or
        (k1 == .number_token) and (k2 == .number_token) or
        (k1 == .bang_token or k1 == .equals_token) and (k2 == .equals_token or k2 == .equals_equals_token);
}

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

const simple_tokens = fixed_tokens ++ &[_]SimpleToken{
    st(.identifier_token, "a"),
    st(.identifier_token, "abc"),
    st(.number_token, "1"),
    st(.number_token, "123"),
};

const separators = &[_]SimpleToken{
    st(.whitespace_token, " "),
    st(.whitespace_token, "  "),
    st(.whitespace_token, "\r"),
    st(.whitespace_token, "\n"),
    st(.whitespace_token, "\r\n"),
};

pub fn lexerTestSuite(state: *t.TestState) void {
    t.runTest(state, void, testsAllTokens, {}, "tests all tokens", .static);

    for (simple_tokens) |tok| {
        t.runTest(
            state,
            SimpleToken,
            lexesToken,
            tok,
            std.fmt.allocPrint(t.allocator, "lexes token {}", .{tok}) catch unreachable,
            .allocated,
        );
    }

    for (simple_tokens) |t1| for (simple_tokens) |t2|
        if (!requiresSeparator(t1.kind, t2.kind))
            t.runTest(
                state,
                [2]SimpleToken,
                lexesTokenPair,
                .{ t1, t2 },
                std.fmt.allocPrint(
                    t.allocator,
                    "lexes token pair {}, {}",
                    .{ t1, t2 },
                ) catch unreachable,
                .allocated,
            );

    for (simple_tokens) |t1| for (simple_tokens) |t2|
        if (requiresSeparator(t1.kind, t2.kind)) for (separators) |sep|
            t.runTest(
                state,
                [3]SimpleToken,
                lexesTokenPairWithSeparator,
                .{ t1, sep, t2 },
                std.fmt.allocPrint(
                    t.allocator,
                    "lexes token pair with separator {}, {}, {}",
                    .{ t1, sep, t2 },
                ) catch unreachable,
                .allocated,
            );
}
