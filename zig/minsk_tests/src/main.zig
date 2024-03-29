const std = @import("std");
const t = @import("framework");
const code_analysis = @import("code_analysis.zig");

pub fn main() !void {
    std.process.exit(try run());
}

fn run() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 10 }){};
    defer _ = gpa.deinit();
    t.allocator = gpa.allocator();

    const args = try std.process.argsAlloc(t.allocator);
    defer std.process.argsFree(t.allocator, args);

    var state = t.TestState{};
    for (args[1..]) |a| {
        if (std.mem.eql(u8, a, "--verbose") or std.mem.eql(u8, a, "-v")) {
            state.verbose = true;
        }
    }

    t.runSuite(
        &state,
        code_analysis.syntax.lexerTestSuite,
        "lexer",
        .static,
    );
    t.runSuite(
        &state,
        code_analysis.syntax.parserTestSuite,
        "parser",
        .static,
    );
    t.runSuite(
        &state,
        code_analysis.syntax.syntaxFactsTestSuite,
        "syntax facts",
        .static,
    );
    t.runSuite(
        &state,
        code_analysis.syntax.evaluatorTestSuite,
        "evaluator",
        .static,
    );
    t.runSuite(
        &state,
        code_analysis.text.sourceTextSuite,
        "source text",
        .static,
    );

    std.debug.print(
        "{d} passed, {d} failed, {d} assertions\n",
        .{ state.passed, state.failed, state.assertions },
    );
    return if (state.failed > 0) 1 else 0;
}
