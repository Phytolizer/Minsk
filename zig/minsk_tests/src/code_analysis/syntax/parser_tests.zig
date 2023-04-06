const std = @import("std");
const t = @import("framework");

const syntax_facts = @import("minsk").code_analysis.syntax.syntax_facts;
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;

fn binary_expression_honors_precedence(state: *t.TestState, ops: [2]SyntaxKind, out_msg: *[]const u8) t.TestExit!void {
    const prec1 = syntax_facts.binaryOperatorPrecedence(ops[0]);
    const prec2 = syntax_facts.binaryOperatorPrecedence(ops[1]);
    const text1 = syntax_facts.getText(ops[0]).?;
    const text2 = syntax_facts.getText(ops[1]).?;
    const text = std.fmt.allocPrint(t.allocator, "a {s} b {s} c", .{ text1, text2 }) catch unreachable;
    defer t.allocator.free(text);

    _ = state;
    _ = out_msg;
    if (prec1 >= prec2) {} else {}
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
