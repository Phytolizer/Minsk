#pragma once

#include "minsk/code_analysis/binding/ast/expressions/binary_operator.h"

typedef struct
{
  struct minsk_bound_node * left;
  minsk_bound_expression_binary_operator_t op;
  struct minsk_bound_node * right;
} minsk_bound_expression_binary_t;

#define MINSK_BOUND_EXPRESSION_BINARY(...) \
  MINSK_BOUND_EXPRESSION( \
    MINSK_BOUND_NODE_TYPE_BINARY_EXPRESSION, \
    .binary = {__VA_ARGS__} \
  )
