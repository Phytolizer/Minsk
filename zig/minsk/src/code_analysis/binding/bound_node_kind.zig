pub const BoundNodeKind = enum {
    assignment_expression,
    binary_expression,
    literal_expression,
    unary_expression,
    variable_expression,

    block_statement,
    expression_statement,
    variable_declaration,
    if_statement,
    while_statement,
};
