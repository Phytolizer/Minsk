pub const binding = @import("code_analysis/binding.zig");
pub const syntax = @import("code_analysis/syntax.zig");
pub const Evaluator = @import("code_analysis/Evaluator.zig");

comptime {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
