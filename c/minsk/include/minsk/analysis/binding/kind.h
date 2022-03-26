#pragma once

#define BOUND_NODE_KINDS_X                                                     \
  X(assignment_expression)                                                     \
  X(binary_expression)                                                         \
  X(literal_expression)                                                        \
  X(unary_expression)                                                          \
  X(variable_expression)

typedef enum {
#define X(x) bound_node_kind_##x,
  BOUND_NODE_KINDS_X
#undef X
} bound_node_kind_t;
