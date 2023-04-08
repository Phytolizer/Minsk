const std = @import("std");
const t = @import("framework");
const Object = @import("minsk_runtime").Object;
const Compilation = @import("minsk").code_analysis.Compilation;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const VariableSymbol = @import("minsk").code_analysis.VariableSymbol;

const EvaluatorTest = struct {
    text: []const u8,
    expected_value: Object,

    pub fn init(text: []const u8, expected_value: Object) @This() {
        return .{ .text = text, .expected_value = expected_value };
    }
};

fn correctEvaluation(state: *t.TestState, tt: EvaluatorTest, out_msg: *[]const u8) t.TestExit!void {
    const syntax_tree = SyntaxTree.parse(t.allocator, tt.text) catch unreachable;
    var compilation = Compilation.init(t.allocator, syntax_tree) catch unreachable;
    defer compilation.deinit();
    var variables = VariableSymbol.Map.init(t.allocator);
    defer variables.deinit();
    const actual_result = compilation.evaluate(&variables) catch unreachable;

    switch (actual_result) {
        .success => |value| try t.assert(
            state,
            value.eq(tt.expected_value),
            "value mismatch ({} != {})",
            .{ value, tt.expected_value },
            out_msg,
        ),
        .failure => |diagnostics| {
            defer t.allocator.free(diagnostics);
            const diagnostic_strings = t.allocator.alloc([]const u8, diagnostics.len) catch unreachable;
            for (diagnostics, diagnostic_strings) |d, *s| {
                s.* = std.fmt.allocPrint(t.allocator, "{s}", .{d}) catch unreachable;
            }
            defer {
                for (diagnostic_strings) |s| {
                    t.allocator.free(s);
                }
                t.allocator.free(diagnostic_strings);
            }
            try t.assert(
                state,
                false,
                "failed with diagnostics:\n{s}\n",
                .{std.mem.join(t.allocator, "\n", diagnostic_strings) catch unreachable},
                out_msg,
            );
        },
    }
}

pub fn evaluatorTestSuite(state: *t.TestState) void {
    const et = EvaluatorTest.init;
    for ([_]EvaluatorTest{
        et("1", .{ .integer = 1 }),
        et("+1", .{ .integer = 1 }),
        et("-1", .{ .integer = -1 }),
        et("1 + 2", .{ .integer = 3 }),
        et("1 - 2", .{ .integer = -1 }),
        et("3 * 3", .{ .integer = 9 }),
        et("9 / 3", .{ .integer = 3 }),
        et("(10)", .{ .integer = 10 }),
        et("12 == 3", .{ .boolean = false }),
        et("3 == 3", .{ .boolean = true }),
        et("12 != 3", .{ .boolean = true }),
        et("3 != 3", .{ .boolean = false }),
        et("true == false", .{ .boolean = false }),
        et("false == false", .{ .boolean = true }),
        et("true != false", .{ .boolean = true }),
        et("true", .{ .boolean = true }),
        et("false", .{ .boolean = false }),
        et("!true", .{ .boolean = false }),
        et("!false", .{ .boolean = true }),
        et("(a = 10) * a", .{ .integer = 100 }),
    }) |tt|
        t.runTest(
            state,
            EvaluatorTest,
            correctEvaluation,
            tt,
            std.fmt.allocPrint(t.allocator, "correct evaluation ({s} => {})", .{ tt.text, tt.expected_value }) catch unreachable,
            .allocated,
        );
}
