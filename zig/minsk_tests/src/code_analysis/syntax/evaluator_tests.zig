const std = @import("std");
const t = @import("framework");
const Object = @import("minsk_runtime").Object;
const Compilation = @import("minsk").code_analysis.Compilation;
const SyntaxTree = @import("minsk").code_analysis.syntax.SyntaxTree;
const VariableSymbol = @import("minsk").code_analysis.VariableSymbol;
const AnnotatedText = @import("AnnotatedText.zig");
const camelToDisplay = @import("../../camel_to_display.zig").camelToDisplay;

const EvaluatorTest = struct {
    text: []const u8,
    expected_value: Object,

    pub fn init(text: []const u8, expected_value: Object) @This() {
        return .{ .text = text, .expected_value = expected_value };
    }
};

fn correctEvaluation(state: *t.TestState, ctx: EvaluatorTest, out_msg: *[]const u8) t.TestExit!void {
    try assertHasValue(state, ctx.text, ctx.expected_value, out_msg);
}

fn assertHasValue(state: *t.TestState, text: []const u8, expected_value: Object, out_msg: *[]const u8) t.TestExit!void {
    const syntax_tree = SyntaxTree.parse(t.allocator, text) catch unreachable;
    var compilation = Compilation.init(t.allocator, syntax_tree) catch unreachable;
    defer compilation.deinit(.with_parents);
    var variables = VariableSymbol.Map.init(t.allocator);
    defer variables.deinit();
    const actual_result = compilation.evaluate(&variables) catch unreachable;
    defer actual_result.deinit(t.allocator);

    switch (actual_result) {
        .success => |value| try t.assert(
            state,
            value.eq(expected_value),
            "value mismatch ({} != {})",
            .{ value, expected_value },
            out_msg,
        ),
        .failure => |diagnostics| {
            const diagnostic_strings = t.allocator.alloc([]const u8, diagnostics.len) catch unreachable;
            for (diagnostics, diagnostic_strings) |d, *s| {
                s.* = std.fmt.allocPrint(t.allocator, "{s}", .{d.message}) catch unreachable;
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
        defer t.allocator.free(diagnostic_strings);
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
            "expected span {}, got {}",
            .{ expected_span, actual_diagnostic.span },
            out_msg,
        );
    }
}

const one_shot_tests = struct {
    pub fn variableDeclarationReportsRedeclaration(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
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

    pub fn blockStatementNoInfiniteLoop(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\[)][]
        ;
        const diagnostics =
            \\Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
            \\Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn nameExpressionNoErrorForInsertedToken(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text = "[]";
        const diagnostics =
            \\Unexpected token <EndOfFileToken>, expected <IdentifierToken>.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn nameExpressionReportsUndefined(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\[x] * 10
        ;
        const diagnostics =
            \\Variable 'x' doesn't exist.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn assignmentExpressionReportsUndefined(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\[x] = 10
        ;
        const diagnostics =
            \\Variable 'x' doesn't exist.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn assignmentReportsCannotAssign(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  let x = 10
            \\  x [=] 0
            \\}
        ;
        const diagnostics =
            \\Variable 'x' is read-only and cannot be assigned to.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn assignmentReportsCannotConvert(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  var x = 10
            \\  x = [true]
            \\}
        ;
        const diagnostics =
            \\Cannot convert type Boolean to type Integer.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn ifStatementReportsCannotConvert(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  if [10]
            \\    var x = true
            \\}
        ;
        const diagnostics =
            \\Cannot convert type Integer to type Boolean.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn whileStatementReportsCannotConvert(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  while [10]
            \\    var x = true
            \\}
        ;
        const diagnostics =
            \\Cannot convert type Integer to type Boolean.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn forStatementReportsCannotConvertLowerBound(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  for i = [true] to 10
            \\    var x = true
            \\}
        ;
        const diagnostics =
            \\Cannot convert type Boolean to type Integer.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn forStatementReportsCannotConvertUpperBound(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\{
            \\  for i = 1 to [false]
            \\    var x = true
            \\}
        ;
        const diagnostics =
            \\Cannot convert type Boolean to type Integer.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn unaryReportsUndefined(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\[+]true
        ;
        const diagnostics =
            \\Unary operator '+' is not defined for type Boolean.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }

    pub fn binaryReportsUndefined(state: *t.TestState, _: void, out_msg: *[]const u8) t.TestExit!void {
        const text =
            \\10 [&&] false
        ;
        const diagnostics =
            \\Binary operator '&&' is not defined for types Integer and Boolean.
        ;
        try assertHasDiagnostics(state, text, diagnostics, out_msg);
    }
};

pub fn evaluatorTestSuite(state: *t.TestState) void {
    const et = EvaluatorTest.init;
    for ([_]EvaluatorTest{
        et("1", .{ .integer = 1 }),
        et("+1", .{ .integer = 1 }),
        et("-1", .{ .integer = -1 }),
        et("~1", .{ .integer = -2 }),
        et("1 + 2", .{ .integer = 3 }),
        et("1 - 2", .{ .integer = -1 }),
        et("3 * 3", .{ .integer = 9 }),
        et("9 / 3", .{ .integer = 3 }),
        et("(10)", .{ .integer = 10 }),
        et("12 ==  3", .{ .boolean = false }),
        et(" 3 ==  3", .{ .boolean = true }),
        et("12 !=  3", .{ .boolean = true }),
        et(" 3 !=  3", .{ .boolean = false }),
        et("3 <  4", .{ .boolean = true }),
        et("5 <  4", .{ .boolean = false }),
        et("4 <= 4", .{ .boolean = true }),
        et("5 <= 4", .{ .boolean = false }),
        et("4 >  3", .{ .boolean = true }),
        et("4 >  5", .{ .boolean = false }),
        et("4 >= 4", .{ .boolean = true }),
        et("4 >= 5", .{ .boolean = false }),
        et("1 | 2", .{ .integer = 3 }),
        et("1 | 0", .{ .integer = 1 }),
        et("1 & 2", .{ .integer = 0 }),
        et("2 & 3", .{ .integer = 2 }),
        et("2 ^ 3", .{ .integer = 1 }),
        et("1 ^ 3", .{ .integer = 2 }),
        et("0 ^ 1", .{ .integer = 1 }),
        et(" true == false", .{ .boolean = false }),
        et("false == false", .{ .boolean = true }),
        et("false != false", .{ .boolean = false }),
        et(" true != false", .{ .boolean = true }),
        et(" true &&  true", .{ .boolean = true }),
        et(" true && false", .{ .boolean = false }),
        et(" true || false", .{ .boolean = true }),
        et("false || false", .{ .boolean = false }),
        et(" true  &  true", .{ .boolean = true }),
        et(" true  & false", .{ .boolean = false }),
        et(" true  | false", .{ .boolean = true }),
        et("false  | false", .{ .boolean = false }),
        et(" true  ^ false", .{ .boolean = true }),
        et("false  ^ false", .{ .boolean = false }),
        et("true   ^  true", .{ .boolean = false }),
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
        et(
            \\{
            \\  var a = 0
            \\  if a == 0
            \\    a = 10
            \\  a
            \\}
        , .{ .integer = 10 }),
        et(
            \\{
            \\  var a = 0
            \\  if a == 4
            \\    a = 10
            \\  a
            \\}
        , .{ .integer = 0 }),
        et(
            \\{
            \\  var a = 0
            \\  if a == 0
            \\    a = 10
            \\  else
            \\    a = 5
            \\  a
            \\}
        , .{ .integer = 10 }),
        et(
            \\{
            \\  var a = 0
            \\  if a == 4
            \\    a = 10
            \\  else
            \\    a = 5
            \\  a
            \\}
        , .{ .integer = 5 }),
        et(
            \\{
            \\  var i = 10
            \\  var result = 0
            \\  while i > 0
            \\  {
            \\    result = result + i
            \\    i = i - 1
            \\  }
            \\  result
            \\}
        , .{ .integer = 55 }),
        et(
            \\{
            \\  var result = 0
            \\  for i = 1 to 10
            \\    result = result + i
            \\  result
            \\}
        , .{ .integer = 55 }),
    }) |tt|
        t.runTest(
            state,
            EvaluatorTest,
            correctEvaluation,
            tt,
            std.fmt.allocPrint(t.allocator, "correct evaluation ({s} => {})", .{ tt.text, tt.expected_value }) catch unreachable,
            .allocated,
        );

    inline for (@typeInfo(one_shot_tests).@"struct".decls) |decl| {
        t.runTest(
            state,
            void,
            @field(one_shot_tests, decl.name),
            {},
            comptime camelToDisplay(decl.name),
            .static,
        );
    }
}
