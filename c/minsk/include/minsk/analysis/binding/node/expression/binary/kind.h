#pragma once

#define BOUND_BINARY_OPERATOR_KINDS_X                                          \
  X(addition)                                                                  \
  X(subtraction)                                                               \
  X(multiplication)                                                            \
  X(division)                                                                  \
  X(logical_and)                                                               \
  X(logical_or)                                                                \
  X(equality)                                                                  \
  X(inequality)

typedef enum {
#define X(x) bound_binary_operator_kind_##x,
  BOUND_BINARY_OPERATOR_KINDS_X
#undef X
} bound_binary_operator_kind_t;
