const std = @import("std");
const snakeToCamel = @import("minsk_meta").snakeToCamel;

pub const SyntaxKind = enum(usize) {
    bad_token,
    end_of_file_token,

    whitespace_token,
    number_token,
    identifier_token,

    plus_token,
    minus_token,
    star_token,
    slash_token,
    open_parenthesis_token,
    close_parenthesis_token,
    open_brace_token,
    close_brace_token,
    bang_token,
    less_token,
    less_equals_token,
    greater_token,
    greater_equals_token,
    ampersand_ampersand_token,
    pipe_pipe_token,
    equals_equals_token,
    bang_equals_token,
    equals_token,

    true_keyword,
    false_keyword,
    let_keyword,
    var_keyword,
    if_keyword,
    else_keyword,

    literal_expression,
    binary_expression,
    parenthesized_expression,
    unary_expression,
    name_expression,
    assignment_expression,

    block_statement,
    expression_statement,
    variable_declaration,
    if_statement,

    compilation_unit,
    else_clause,

    const Self = @This();

    const display_names = blk: {
        const fields = std.meta.fields(Self);
        var result: [fields.len][]const u8 = undefined;
        for (&result, 0..) |*r, i| {
            r.* = snakeToCamel(fields[i].name);
        }
        break :blk result;
    };

    pub fn displayName(self: Self) []const u8 {
        return display_names[@enumToInt(self)];
    }
};
