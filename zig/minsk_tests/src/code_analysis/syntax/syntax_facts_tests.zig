const std = @import("std");
const t = @import("framework");
const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxToken = @import("minsk").code_analysis.syntax.SyntaxToken;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;

fn get_text_round_trip(state: *t.TestState, kind: SyntaxKind, out_msg: *[]const u8) t.TestExit!void {
    const text = syntax_facts.getText(kind) orelse return error.TestSkipped;

    const tokens = SyntaxTree.parseTokens(t.allocator, text) catch unreachable;
    defer t.allocator.free(tokens);
    const token = try t.assertSingle(
        state,
        SyntaxToken,
        tokens,
        "too many tokens ({d})",
        .{tokens.len},
        out_msg,
    );

    try t.assert(
        state,
        token.kind == kind,
        "kind mismatch ({s} != {s})",
        .{ token.kind.displayName(), kind.displayName() },
        out_msg,
    );
    try t.assert(
        state,
        std.mem.eql(u8, token.text, text),
        "text mismatch ({s} != {s})",
        .{ token.text, text },
        out_msg,
    );
}

pub fn syntax_facts_test_suite(state: *t.TestState) void {
    for (std.meta.tags(SyntaxKind)) |k| {
        t.runTest(
            state,
            SyntaxKind,
            get_text_round_trip,
            k,
            std.fmt.allocPrint(
                t.allocator,
                "getText round trips ({s})",
                .{k.displayName()},
            ) catch unreachable,
            .allocated,
        );
    }

    for (syntax_facts.binaryOperators()) |op| {
        _ = op;
    }
}
