const std = @import("std");
const t = @import("framework");
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const SyntaxToken = @import("minsk").code_analysis.syntax.SyntaxToken;
const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;
const token = @import("token.zig");
const SimpleToken = token.SimpleToken;

fn testToken(
    state: *t.TestState,
    tok: SyntaxToken,
    exp: SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    try t.assert(
        state,
        tok.kind == exp.kind,
        "kind mismatch: {s} != {s}",
        .{ tok.kind.displayName(), exp.kind.displayName() },
        out_msg,
    );
    try t.assert(
        state,
        std.mem.eql(u8, tok.text, exp.text),
        "text mismatch: '{s}' != '{s}'",
        .{ tok.text, exp.text },
        out_msg,
    );
}

fn requireTestsAllTokens() void {
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
        for (token.simple ++ token.separators) |tok|
            result = result ++ &[_]SyntaxKind{tok.kind};
        break :blk result;
    };

    const messages = comptime blk: {
        var result: []const []const u8 = &.{};
        for (token_kinds) |tk| {
            @setEvalBranchQuota(10000);
            if (std.mem.indexOfScalar(SyntaxKind, tested_token_kinds, tk) == null) {
                result = result ++ &[_][]const u8{
                    std.fmt.comptimePrint("missing token kind: {s}", .{tk.displayName()}),
                };
            }
        }
        break :blk result;
    };
    if (messages.len > 0) {
        var errmsg: []const u8 = "ERROR: not all token types are tested:";
        for (messages) |msg| {
            errmsg = errmsg ++ "\n> " ++ msg;
        }
        @compileError(errmsg);
    }
}

fn lexesToken(
    state: *t.TestState,
    ctx: SimpleToken,
    out_msg: *[]const u8,
) t.TestExit!void {
    const tokens = SyntaxTree.parseTokens(t.allocator, ctx.text) catch unreachable;
    defer t.allocator.free(tokens);

    const tok = try t.assertSingle(
        state,
        SyntaxToken,
        tokens,
        "too many tokens ({d})",
        .{tokens.len},
        out_msg,
    );
    try testToken(state, tok, ctx, out_msg);
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
        ((k1 == .bang_token or k1 == .equals_token or k1 == .less_token or k1 == .greater_token) and
        (k2 == .equals_token or k2 == .equals_equals_token));
}

comptime {
    requireTestsAllTokens();
}

pub fn lexerTestSuite(state: *t.TestState) void {
    for (token.simple) |tok| {
        t.runTest(
            state,
            SimpleToken,
            lexesToken,
            tok,
            std.fmt.allocPrint(t.allocator, "lexes token {}", .{tok}) catch unreachable,
            .allocated,
        );
    }

    for (token.simple) |t1| for (token.simple) |t2|
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

    for (token.simple) |t1| for (token.simple) |t2|
        if (requiresSeparator(t1.kind, t2.kind)) for (token.separators) |sep|
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
