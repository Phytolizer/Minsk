#pragma once

#define BOUND_UNARY_OPERATOR_KINDS_X \
  X(identity) \
  X(negation) \
  X(logical_negation)

typedef enum {
#define X(x) bound_unary_operator_kind_##x,
  BOUND_UNARY_OPERATOR_KINDS_X
#undef X
} bound_unary_operator_kind_t;
