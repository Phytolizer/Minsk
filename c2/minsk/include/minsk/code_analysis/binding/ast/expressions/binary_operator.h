#pragma once

#include "minsk/meta/concat.h"

#define MINSK_PREFIX_BOUND_EXPRESSION_BINARY_OPERATOR_KIND \
  MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_

typedef enum
{
#define X(x) \
  MINSK_CONCAT(MINSK_PREFIX_BOUND_EXPRESSION_BINARY_OPERATOR_KIND, x),
#include "./private/binary_operator_kinds.xmacro"
#undef X
} minsk_bound_expression_binary_operator_kind_t;
