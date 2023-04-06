const std = @import("std");
const t = @import("framework");

const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;

const AssertingEnumerator = @import("AssertingEnumerator.zig");

fn binary_expression_honors_precedence(state: *t.TestState, ops: [2]SyntaxKind, out_msg: *[]const u8) t.TestExit!void {
    const prec1 = syntax_facts.binaryOperatorPrecedence(ops[0]);
    const prec2 = syntax_facts.binaryOperatorPrecedence(ops[1]);
    const text1 = syntax_facts.getText(ops[0]).?;
    const text2 = syntax_facts.getText(ops[1]).?;
    const text = std.fmt.allocPrint(t.allocator, "a {s} b {s} c", .{ text1, text2 }) catch unreachable;
    defer t.allocator.free(text);

    const expression = SyntaxTree.parse(t.allocator, text) catch unreachable;
    defer expression.deinit();

    if (prec1 >= prec2) {
        var e = AssertingEnumerator.init(t.allocator, &expression.root.base) catch unreachable;
        defer e.deinit();

        try e.assertNode(state, .binary_expression, out_msg);
        try e.assertNode(state, .binary_expression, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "a", out_msg);
        try e.assertToken(state, ops[0], text1, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "b", out_msg);
        try e.assertToken(state, ops[1], text2, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "c", out_msg);
        try e.assertEnd(state, out_msg);
    } else {
        var e = AssertingEnumerator.init(t.allocator, &expression.root.base) catch unreachable;
        defer e.deinit();

        try e.assertNode(state, .binary_expression, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "a", out_msg);
        try e.assertToken(state, ops[0], text1, out_msg);
        try e.assertNode(state, .binary_expression, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "b", out_msg);
        try e.assertToken(state, ops[1], text2, out_msg);
        try e.assertNode(state, .name_expression, out_msg);
        try e.assertToken(state, .identifier_token, "c", out_msg);
        try e.assertEnd(state, out_msg);
    }
}

pub fn parser_test_suite(state: *t.TestState) void {
    for (syntax_facts.binaryOperators()) |op1| for (syntax_facts.binaryOperators()) |op2|
        t.runTest(
            state,
            [2]SyntaxKind,
            binary_expression_honors_precedence,
            .{ op1, op2 },
            std.fmt.allocPrint(
                t.allocator,
                "binary expression honors precedence ({s}, {s})",
                .{ op1.displayName(), op2.displayName() },
            ) catch unreachable,
            .allocated,
        );
}
