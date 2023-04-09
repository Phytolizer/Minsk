const std = @import("std");
const t = @import("framework");
const Object = @import("minsk_runtime").Object;
const Compilation = @import("minsk").code_analysis.Compilation;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const VariableSymbol = @import("minsk").code_analysis.VariableSymbol;
const AnnotatedText = @import("AnnotatedText.zig");

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
    defer compilation.deinit(.with_parents);
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
            defer {
                for (diagnostics) |d| d.deinit(t.allocator);
                t.allocator.free(diagnostics);
            }
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
            const msgs = std.mem.join(t.allocator, "\n", diagnostic_strings) catch unreachable;
            defer t.allocator.free(msgs);
            try t.assert(
                state,
                false,
                "failed with diagnostics:\n{s}\n",
                .{msgs},
                out_msg,
            );
        },
    }
}

fn assertHasDiagnostics(
    state: *t.TestState,
    comptime text: []const u8,
    comptime diagnostic_text: []const u8,
    out_msg: *[]const u8,
) t.TestExit!void {
    const annotated_text = comptime AnnotatedText.parse(text);
    const syntax_tree = SyntaxTree.parse(t.allocator, annotated_text.text) catch unreachable;
    const compilation = Compilation.init(t.allocator, syntax_tree) catch unreachable;
    defer compilation.deinit(.with_parents);
    var variables = VariableSymbol.Map.init(t.allocator);
    defer variables.deinit();
    const result = compilation.evaluate(&variables) catch unreachable;
    defer result.deinit(t.allocator);

    const diagnostics = comptime AnnotatedText.unindentLines(diagnostic_text);
    if (annotated_text.spans.len != diagnostics.len) {
        @compileError(std.fmt.comptimePrint(
            "must mark as many spans as there are diagnostics ({d} != {d})",
            .{ annotated_text.spans.len, diagnostics.len },
        ));
    }

    const actual_diagnostics = switch (result) {
        .success => |val| return t.assert(state, false, "expected failure, got {}", .{val}, out_msg),
        .failure => |d| d,
    };

    if (actual_diagnostics.len != diagnostics.len) {
        const diagnostic_strings = blk: {
            var strings = std.ArrayList([]const u8).init(t.allocator);
            defer {
                for (strings.items) |s|
                    t.allocator.free(s);
                strings.deinit();
            }
            for (actual_diagnostics) |ad| {
                strings.append(
                    std.fmt.allocPrint(t.allocator, "{d}: {s}", .{ ad.span.start, ad.message }) catch unreachable,
                ) catch unreachable;
            }
            break :blk std.mem.join(t.allocator, "\n", strings.items) catch unreachable;
        };
        try t.assert(
            state,
            false,
            "actual diagnostics do not match:\n{s}",
            .{diagnostic_strings},
            out_msg,
        );
    }

    for (annotated_text.spans, diagnostics, actual_diagnostics) |
        expected_span,
        expected_message,
        actual_diagnostic,
    | {
        try t.assert(
            state,
            std.mem.eql(u8, expected_message, actual_diagnostic.message),
            \\diagnostic mismatch
            \\expected: {s}
            \\actually: {s}
        ,
            .{ expected_message, actual_diagnostic.message },
            out_msg,
        );

        try t.assert(
            state,
            std.meta.eql(expected_span, actual_diagnostic.span),
            "expected span {d}..{d}, got {d}..{d}",
            .{
                expected_span.start,
                expected_span.end(),
                actual_diagnostic.span.start,
                actual_diagnostic.span.end(),
            },
            out_msg,
        );
    }
}

fn variableDeclarationReportsRedeclaration(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
    const text =
        \\{
        \\  var x = 10
        \\  var y = 100
        \\  {
        \\    var x = 10
        \\  }
        \\  var [x] = 5
        \\}
    ;
    const diagnostics =
        \\Variable 'x' has already been declared.
    ;
    try assertHasDiagnostics(state, text, diagnostics, out_msg);
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
        et(
            \\{
            \\  var a = 0
            \\  (a = 10) * a
            \\}
        , .{ .integer = 100 }),
    }) |tt|
        t.runTest(
            state,
            EvaluatorTest,
            correctEvaluation,
            tt,
            std.fmt.allocPrint(t.allocator, "correct evaluation ({s} => {})", .{ tt.text, tt.expected_value }) catch unreachable,
            .allocated,
        );

    t.runTest(
        state,
        void,
        variableDeclarationReportsRedeclaration,
        {},
        "variable declaration reports redeclaration",
        .static,
    );
}
